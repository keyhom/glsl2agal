#ifndef _GLSL_LOADER_H
#define _GLSL_LOADER_H

#include <vector>
#include <unordered_map>
#include <string>

//! @class ShaderDependencyNode
//
class ShaderDependencyNode {
public:

    ShaderDependencyNode(const std::string& shaderName);
    ~ShaderDependencyNode();

    const std::string& getName() const { return shaderName_; }
    void setName(const std::string& name) { shaderName_ = name; }

    const std::string& getSource() const { return shaderSource_; }
    void setSource(const std::string& source) { shaderSource_ = source; }

    void addDependency(int index, ShaderDependencyNode* node);
    void removeDependency(ShaderDependencyNode* node);

    const std::vector<ShaderDependencyNode*>& getDependOnMe() const {
        return this->dependOnMe_;
    }

    const std::vector<ShaderDependencyNode*>& getDependencies() const {
        return this->dependencies_;
    }

    const std::vector<int>& getDependencyInjectIndices() const {
        return this->dependencyInjectIndices_;
    }

private:

    std::string shaderName_;
    std::string shaderSource_;

    std::vector<ShaderDependencyNode*> dependencies_;
    std::vector<int> dependencyInjectIndices_;
    std::vector<ShaderDependencyNode*> dependOnMe_;

}; // class ShaderDependencyNode

//! @class GLSLLoader
//
// GLSL file parser that supports #import pre-processor statement.
//
class GLSLLoader {

public:

    typedef std::unordered_map<std::string, ShaderDependencyNode*> ShaderDependencyMap;

    explicit GLSLLoader(const std::string &libPath);
    ~GLSLLoader();

    std::string load(const std::string &filename);
    std::string loadContent(const std::string &textContent);

protected:
    ShaderDependencyNode* loadNode(const std::string& content, bool root, const std::string& name);

private:

    std::string resolveDependencies(ShaderDependencyNode* node);
    ShaderDependencyNode* nextIndependentNode() const;

private:

    std::string libPath_;
    ShaderDependencyMap dependCache_;
    std::vector<ShaderDependencyNode*> resolvedNodes_;

}; // class GLSLLoader

#endif // _GLSL_LOADER_H
