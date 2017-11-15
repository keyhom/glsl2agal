#ifndef _AGAL_COMPILER_H
#define _AGAL_COMPILER_H

#include <string>
#include <glsl_optimizer.h>
#include <standalone_scaffolding.h>

namespace agal {

class AGALCompiler {
public:

    AGALCompiler();
    virtual ~AGALCompiler();

    bool init(int agalVersion, int glesVersion = 0);
    bool compile(int type, const char *shaderSource, bool optimize, bool skipPreprocessor);

    const std::string getOutput() const { return output_; }

private:
    glslopt_ctx *ctx_;
    glslopt_target glTarget_;
    std::string output_;

}; // class AGALCompiler

}; // namespace agal

#endif // _AGAL_COMPILER_H
