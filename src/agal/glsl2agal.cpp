#include <assert.h>
#include <glsl_optimizer.h>
#include <standalone_scaffolding.h>
#include <AS3/AS3.h>

static glslopt_ctx *gContext = NULL;
static glslopt_target gTarget;

extern "C" void glsl2agal_init(int gles) {
    gContext = glslopt_initialize(gTarget);
    assert(gContext && "Failed to initialize glslopt context.");
}

extern "C" void glsl2agal_compile(int shaderType, char *shaderSource, int options) {
    const glslopt_shader_type vShaderType = shaderType ? kGlslOptShaderVertex : kGlslOptShaderFragment;
    glslopt_shader *shader = glslopt_optimize(gContext, vShaderType, shaderSource, options);

    const char *optimizedShader = glslopt_get_output(shader);

    AS3_DeclareVar(outputStr, String);
    AS3_CopyCStringToVar(outputStr, optimizedShader, strlen(optimizedShader));

    inline_as3(
        "import com.adobe.AGALOptimiser.translator.transformations.Utils;\n"
        "if (optimize) {\n"
        "    var shader:Object = null;\n"
        "    try { shader = JSON.parse(outputStr); } catch (e:*) {}\n"
        "    if (shader != null && shader[\"agalasm\"] != null) {\n"
        "        shader = Utils.optimizeShader(shader, mode == 0);\n"
        "        outputStr = JSON.stringify(shader, null, 2);\n"
        "    }\n"
        "}\n"
    );

    glslopt_shader_delete(shader);
}


