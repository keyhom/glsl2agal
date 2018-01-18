#include "glsl_loader.h"
#include <cassert>
#include <cctype>
#include <algorithm>
#include <iterator>
#include <functional>
#include <sstream>
#include <fstream>

namespace {

std::string getTextContent(const std::string &filename, const std::string &searchPath = "", bool subNode = true) {
    std::string ret;
    std::ifstream is;
    std::string file_fullname;
    if (subNode && !searchPath.empty()) {
        file_fullname = searchPath + '/' + filename;
    }
    else {
        file_fullname = filename;
    }
    is.open(file_fullname, std::ios::in);
    if (is.is_open()) {
        ret = std::string(std::istreambuf_iterator<char>(is), std::istreambuf_iterator<char>());
    }
    is.close();
    return ret;
}

std::string& trim(std::string &line) {
    auto li = std::find_if(line.begin(), line.end(), [](int a) { return !std::isspace(a); });
    line.erase(line.begin(), li);

    auto ri = std::find_if(line.rbegin(), line.rend(), [](int a) { return !std::isspace(a); });
    line.erase(ri.base(), line.end());
    return line;
}

}; // namespace

GLSLLoader::GLSLLoader(const std::string &libPath)
    : libPath_(libPath)
{

}

GLSLLoader::~GLSLLoader() {
    auto p = this->dependCache_.begin();
    for (; p != this->dependCache_.end(); ++p) {
        auto *node = p->second;
        delete node;
    }
    this->dependCache_.clear();
    this->resolvedNodes_.clear();
}

ShaderDependencyNode *GLSLLoader::loadNode(const std::string &content, bool root, const std::string &name) {
    ShaderDependencyNode *node = new ShaderDependencyNode(name);
    std::string sb;

    // Append comment.
    if (!root) {
        sb.append("// -- begin import ").append(name).append(" --\n");
    }

    std::stringstream ss(content);
    std::string line;

    while (std::getline(ss, line)) {
        if (line.empty())
            continue;

        if (trim(line).substr(0, 8) == "#import ") {
            size_t pos = line.rfind('\r');
            if (pos != std::string::npos) {
                line = line.substr(0, pos);
            }
            line = trim(line);
            line = line.substr(8);

            if (line.length() > 3 && '"' == line.at(0) && line.at(line.size() - 1) == '"') {
                // import user code.
                // remove quotes to get filename.
                line = line.substr(1, line.length() - 2);
                if (line == name) {
                    assert(false && "Node depends on itself.");
                }

                // check cache first.
                ShaderDependencyMap::iterator cache_iter = dependCache_.find(line);
                ShaderDependencyNode *dependNode = nullptr;
                if (cache_iter == this->dependCache_.end()) {
                    dependNode = this->loadNode(getTextContent(line, this->libPath_, true), false, line);
                }
                else {
                    dependNode = cache_iter->second;
                }

                if (dependNode)
                    node->addDependency(sb.size(), dependNode);
            }
        }
        else {
            sb.append(line).append(1, '\n');
        }
    }

    if (!root) {
        sb.append("// -- end import ").append(name).append(" --\n");
    }

    node->setSource(sb);
    dependCache_.insert(std::make_pair(name, node));

    return node;
}

std::string GLSLLoader::load(const std::string &filename) {
    if (filename.empty()) {
        return nullptr;
    }

    if (filename.rfind(".glsllib") != std::string::npos) {
        assert(false && ".glsllib must be load by the default loader.");
    }

    ShaderDependencyNode *rootNode = this->loadNode(getTextContent(filename, this->libPath_, false), true, "");
    if (rootNode) {
        std::string ret = this->resolveDependencies(rootNode);
        this->resolvedNodes_.clear();
        this->dependCache_.clear();
        return ret;
    }

    return nullptr;
}

std::string GLSLLoader::loadContent(const std::string &textContent) {
    if (textContent.empty())
        return nullptr;

    ShaderDependencyNode *rootNode = this->loadNode(textContent, true, "");
    if (rootNode) {
        std::string ret = this->resolveDependencies(rootNode);
        this->resolvedNodes_.clear();
        this->dependCache_.clear();
        return ret;
    }

    return nullptr;
}

std::string GLSLLoader::resolveDependencies(ShaderDependencyNode *node) {
    std::vector<ShaderDependencyNode*>::iterator iter = std::find(resolvedNodes_.begin(), resolvedNodes_.end(), node);
    if (iter != resolvedNodes_.end()) {
        return "// " + node->getName() + " was already injected at the top.\n";
    } else {
        resolvedNodes_.push_back(node);
    }

    if (node->getDependencies().empty()) {
        return node->getSource();
    } else {
        std::string sb(node->getSource());
        std::vector<std::string> resolveShaderNodes;
        std::vector<ShaderDependencyNode *>::const_iterator iter = node->getDependencies().begin();
        for (; iter != node->getDependencies().end(); ++iter) {
            resolveShaderNodes.push_back(this->resolveDependencies(*iter));
        }
        const std::vector<int> &injectIndies = node->getDependencyInjectIndices();
        int32_t size = injectIndies.size();
        for (int32_t i = size - 1; i >= 0; --i) {
            // Must insert them backwards...
            sb.insert(injectIndies.at(i), resolveShaderNodes.at(i));
        }

        return sb;
    }

    return nullptr;
}

ShaderDependencyNode::ShaderDependencyNode(const std::string &name)
    : shaderName_(name)
{

}

ShaderDependencyNode::~ShaderDependencyNode() {

}

void ShaderDependencyNode::addDependency(int index, ShaderDependencyNode *node) {
    if (this->dependencies_.end() != std::find(this->dependencies_.begin(), this->dependencies_.end(), node)) {
        // already contains dependency.
        return;
    }

    this->dependencies_.push_back(node);
    this->dependencyInjectIndices_.push_back(index);
    node->dependOnMe_.push_back(this);
}

