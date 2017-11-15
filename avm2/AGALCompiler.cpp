#include "AGALCompiler.h"
#include <AS3/AS3.h>

namespace agal {

AGALCompiler::AGALCompiler()
    : ctx_(NULL)
    , glTarget_(kGlslTargetOpenGL)
    , output_()
{

}

AGALCompiler::~AGALCompiler() {
    glslopt_cleanup(this->ctx_);
    this->ctx_ = NULL;
}

bool AGALCompiler::init(int agalVersion, int glesVersion) {
    // TODO: select the mesa caps via agal version and gles version.
    if (NULL == ctx_) {
        ctx_ = glslopt_initialize(this->glTarget_);
    }

    return NULL != ctx_;
}

bool AGALCompiler::compile(int type, const char *shaderSource, bool optimize, bool skipPreprocessor) {
    glslopt_shader_type shaderType;
    switch (type) {
        case 0:
            shaderType = kGlslOptShaderVertex;
            break;
        case 1:
            shaderType = kGlslOptShaderFragment;
            break;
        default:
            assert(false && "Unsupported shader type for AGAL compiler.");
            return false;
    }

    if (NULL == shaderSource)
        return false;

    unsigned options = 0;
    if (skipPreprocessor)
        options |= kGlslOptionSkipPreprocessor;

    options |= kGlslOptionToAgalShader;

    glslopt_shader *shader = glslopt_optimize(this->ctx_, shaderType, shaderSource, options);

    bool valid = glslopt_get_status(shader);
    const char *agalobj_str = glslopt_get_output(shader);

    if (valid && optimize) {
        AS3_DeclareVar(outputstr, String);
        inline_as3(
            "import com.adobe.AGALOptimiser.translator.transformations.Utils;\n"
            "var shader:Object = null;\n"
            "var sPtr:int = %0;\n"
            "outputstr = CModule.readString(sPtr, %1);\n"
            "try { shader = JSON.parse(outputstr) } catch(e:Error) { trace('// !!- JSON.parse ERROR: ', e.errorID, e.message); }\n"
            "if(shader != null && shader[\"agalasm\"] != null) {\n"
            "   try {\n"
            "       shader = Utils.optimizeShader(shader, %2 == 0);\n"
            "       outputstr = JSON.stringify(shader, null, 1);\n"
            "       CModule.writeString(sPtr, outputstr);\n"
            "       CModule.write8(sPtr + outputstr.length, 0);\n"
            "   } catch (e:Error) {}\n"
            "}\n"
            :: "r"(agalobj_str), "r"(strlen(agalobj_str)), "r"(type)
        );
    }

    this->output_.assign(agalobj_str);

    glslopt_shader_delete(shader);

    return valid;
}

}; // namespace agal

