#include <getopt.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef __AVM2__
#include <AS3/AS3.h>
#endif // __AVM2__
#include "glsl_optimizer.h"
#include "standalone_scaffolding.h"
#include "glslopt_util.h"
#include "glsl_loader.h"

std::chrono::steady_clock::time_point epoch = std::chrono::steady_clock::now();

static glslopt_ctx *gContext = NULL;
int gAGAL = 0;
int gOptimized = 0;
int gGles = 0;
int gVerbose = 0;

#ifndef SWC
bool gVertex = false;
bool gFreename = false;
int gPreprocessor = false;
char *gLibPath = NULL;

const char *const short_options = "hvfe:";
const struct option long_options[] = {
   {"help",           no_argument,         NULL,             'h'},
   {"vertex",         no_argument,         NULL,             'v'},
   {"fragment",       no_argument,         NULL,             'f'},
   {"agal",           no_argument,         &gAGAL,           1},
   {"optimize",       no_argument,         &gOptimized,      1},
   {"gles",           optional_argument,   &gGles,           1},
   {"preprocessor",   no_argument,         &gPreprocessor,   1},
   {"libpath",        required_argument,   NULL,             0},
   {"verbose",        no_argument,         &gVerbose,        1},
   {0,                0,                   0,                0}
};

static int showUsage(const char *msg) {
    if (msg)
        fprintf(stderr, "%s\n\n\n", msg);

    const char *const usages = {
        "\nUsage: glslopt [options] ... <input shader(s)> [<output shader/dir>]\n"
        "\n"
        "\t-v,\t--vertex\t\tVertex shader.\n"
        "\t-f,\t--fragment\t\tFragment shader.\n"
        "\t-e,\t--gles\t\t\tSet GLES specification, the option value can be, 2, 3.\n"
        "\n"
        "\t--agal\t\t\t\tEmit to AGAL asm output.\n"
        "\t--optimize\t\t\tRun optimization passes.\n"
        "\t--preprocessor\t\t\tRun pre-processor passes.\n"
        "\t--libpath\t\t\tA searching path for shader lib.\n"
        "\nIf no output specified, output is to [input(s)].out.\n"
    };

    fprintf(stderr, "%s", usages);
    return EXIT_FAILURE;
}

static char *loadFile(const char *filename) {
    FILE *file = fopen(filename, "rt");
    if (!file) {
        fprintf(stderr, "Failed to open %s for reading\n", filename);
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *result = new char[size + 1];
    result[size] = 0;

    size_t count = fread(result, 1, size, file);
    result[count] = 0;

    fclose(file);
    return result;
}

static bool saveFile(const char* filename, const char* data)
{
	size_t size = strlen(data);

	FILE *file = fopen(filename, "wt");
	if( !file )
	{
		fprintf(stderr, "Failed to open %s for writing\n", filename);
		return false;
	}

	if( 1 != fwrite(data, size, 1, file) )
	{
		fprintf(stderr, "Failed to write to %s\n", filename);
		fclose(file);
		return false;
	}

	fclose(file);
	return true;
}

#endif // SWC

static void glslopt_destroy_context() {
    if (gContext)
        glslopt_cleanup(gContext);
    gContext = NULL;
}

#ifdef SWC
extern "C" void compileShader() __attribute__((used, annotate(
    "as3sig:public function compileShader(src:String, mode:int, optimize:Boolean, gles:int = 1, preprocessor:Boolean = false, debugMode:Boolean = false):Object"
)));

extern "C" void compileShader() {
    // Copy the AS3 string to the C heap
    char *src = NULL;
    AS3_MallocString(src, src);

    int mode = 0;
    AS3_GetScalarFromVar(mode, mode);

    int gles = 0;
    AS3_GetScalarFromVar(gles, gles);

    bool optimize = false;
    AS3_GetScalarFromVar(optimize, optimize);

    bool preprocessor = false;
    AS3_GetScalarFromVar(preprocessor, preprocessor);

    bool debugMode = false;
    AS3_GetScalarFromVar(debugMode, debugMode);

    const glslopt_shader_type shaderType = mode == 0 ? kGlslOptShaderVertex : kGlslOptShaderFragment;

    if (gContext) {
        if (gles != gGles) {
            glslopt_destroy_context();
        }
    }

    gGles = gles;
    gOptimized = optimize;
    gAGAL = 1;

    GLSLLoader srcLoader("");
    std::string originalContent = srcLoader.loadContent(src);
    const char *originalShader = originalContent.c_str();

    // printf("// ### The original src: \n%s\n", src);
    // printf("\n\n");
    // printf("// ### The original shader: \n%s\n", originalShader);

    delete[] src;

#else
bool compileShader(const char *dstFilename, const char *srcFilename, bool vertexShader, bool preprocessor = false) {
    const glslopt_shader_type shaderType = vertexShader ? kGlslOptShaderVertex : kGlslOptShaderFragment;

    bool debugMode = false;

#   ifdef __AVM2__
    AS3_DeclareVar(optimize, Boolean);
    AS3_DeclareVar(mode, int);

    AS3_CopyScalarToVar(optimize, gOptimized);
    AS3_CopyScalarToVar(mode, !vertexShader);
#   endif

    GLSLLoader srcLoader(gLibPath ? gLibPath : "");
    std::string originalContent = srcLoader.load(srcFilename);
    if (gVerbose && !originalContent.empty()) {
        fprintf(stderr, "// ### Expand GLSL ###\n\n");
        fprintf(stderr, "%s\n", originalContent.c_str());
        fprintf(stderr, "//-------------------------------------------------------------------------- END\n\n");
    }

    //const char *originalShader = loadFile(srcFilename);
    const char *originalShader = originalContent.c_str();
    if (!originalShader)
        return false;
#endif // SWC

    StopWatch w;

    glslopt_target target;
    if (gGles == 2)
        target = kGlslTargetOpenGLES20;
    else if (gGles == 3)
        target = kGlslTargetOpenGLES30;
    else
        target = kGlslTargetOpenGL;

    if (NULL == gContext) {
        w.start();
        gContext = glslopt_initialize(target);
        w.stop();
        printf("glslopt_initialize, time usage: %d ms\n", w.getElapsedTime());
    }

    unsigned options = 0;
    if (!preprocessor)
        options |= kGlslOptionSkipPreprocessor;

    if (gAGAL) {
        options |= kGlslOptionToAgalShader;
    }

    if (debugMode)
        options |= kGlslOptionDebugInfo;

    w.start();
    glslopt_shader *shader = glslopt_optimize(gContext, shaderType, originalShader, options);
    w.stop();

    printf("glslopt_optimize, time usage: %d ms\n", w.getElapsedTime());

    if (gVerbose) {
        fprintf(stderr, "\n// ### RAW ###\n\n");
        fprintf(stderr, "%s\n", glslopt_get_raw_output(shader));
        fprintf(stderr, "//-------------------------------------------------------------------------- END\n\n");
    }

#ifndef SWC
    if (!glslopt_get_status(shader)) {
        fprintf(stderr, "%s\n", glslopt_get_log(shader));
        return false;
    }
#endif

    const char *optimizedShader = glslopt_get_output(shader);

#ifdef __AVM2__
    AS3_DeclareVar(shader, Object);
    AS3_DeclareVar(outputStr, String);
    AS3_CopyCStringToVar(outputStr, optimizedShader, strlen(optimizedShader));

    inline_as3(
        "import com.adobe.AGALOptimiser.translator.transformations.Utils;\n"
        "if (optimize) {\n"
        "    try { shader = JSON.parse(outputStr); } catch (e:*) { trace('Error parsing JSON: ', e.toString()); }\n"
        "    if (shader != null && shader[\"agalasm\"] != null) {\n"
        "        var optShader:Object = Utils.optimizeShader(shader, mode == 0);\n"
        "        for (var k:String in optShader) {\n"
        "            shader[k] = optShader[k];"
        "        }\n"
        "        // shader = JSON.stringify(shader, null, 2);\n"
        "    }\n"
        "} else {\n"
        "   shader = outputStr;\n"
        "}\n"
    );

#endif // __AVM2__

#ifdef SWC
    glslopt_shader_delete(shader);
    AS3_ReturnAS3Var(shader);
#else

    if (dstFilename && !saveFile(dstFilename, optimizedShader)) {
        return false;
    } else if (NULL == dstFilename) {
        printf("%s\n", optimizedShader);
    }

    glslopt_shader_delete(shader);

    //delete[] originalShader;
    return true;
#endif
}

int main(int argc, char **argv) {
#ifdef SWC
    AS3_GoAsync();
#else
    if (argc == 0) {
        return showUsage("Arguments required!");
    }

    int option_idx = -1;
    int ch = -1;
    bool emitType = false;

    while (-1 != (ch = getopt_long(argc, argv, short_options, long_options, &option_idx))) {
        switch (ch) {
            case 'v':
                gVertex = true;
                emitType = true;
                break;
            case 'f':
                gVertex = !!gVertex;
                emitType = true;
                break;
            case 'e':
                gGles = ::atoi(optarg);
                break;
            case 'h':
                return showUsage(NULL);
            case '?':
            default:
                if (option_idx >= 0) {
                    const struct option &o = long_options[option_idx];
                    if (!strcmp(o.name, "libpath")) {
                        gLibPath = optarg;
                    }
                }
                break;
        }
    }

    if (argc <= optind) {
        return showUsage("No input shader(s).");
    }

    if (!emitType) {
        return showUsage("No shader type to emit.");
    }

    const char *srcFilename = argv[optind++];
    char *destFilename = NULL;

    if (argc <= optind) {
        gFreename = true;
    } else {
        destFilename = argv[optind];
    }

    optind = 0;
    opterr = 0;

    int result = 0;
    if (!compileShader(destFilename, srcFilename, gVertex, gPreprocessor)) {
        result = 1;
    }

    glslopt_destroy_context();
    return result;
#endif // SWC
}


