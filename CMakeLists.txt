cmake_minimum_required(VERSION 2.8)

project(glsl2agal)

include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_SOURCE_DIR}/src/mesa)
include_directories(${PROJECT_SOURCE_DIR}/src/mapi)
include_directories(${PROJECT_SOURCE_DIR}/src/glsl)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O4 -DUNIX")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O4 -DUNIX")

file(GLOB agalasm_sources ${PROJECT_SOURCE_DIR}/agalassembler/*.cpp)
add_library(agalasm ${agalasm_sources})

file(GLOB glcpp-library_sources ${PROJECT_SOURCE_DIR}/src/glsl/glcpp/*.c)
file(GLOB glcpp-library_sources_remove ${PROJECT_SOURCE_DIR}/src/glsl/glcpp/glcpp.c)
list(REMOVE_ITEM glcpp-library_sources ${glcpp-library_sources_remove})
add_library(glcpp-library ${glcpp-library_sources})

file(GLOB mesa_sources ${PROJECT_SOURCE_DIR}/src/mesa/program/*.c)
add_library(mesa ${mesa_sources})

file(GLOB glsl_sources ${PROJECT_SOURCE_DIR}/src/glsl/*.cpp src/glsl/*.c)
file(GLOB glsl_sources_remove ${PROJECT_SOURCE_DIR}/src/glsl/main.cpp ${PROJECT_SOURCE_DIR}/src/glsl/builtin_stubs.cpp)
list(REMOVE_ITEM glsl_sources ${glsl_sources_remove})
add_library(glsl_optimizer ${glsl_sources})
target_link_libraries(glsl_optimizer glcpp-library mesa agalasm)

file(GLOB glslopt_sources ${PROJECT_SOURCE_DIR}/contrib/glslopt/*.cpp)
add_executable(glsl2agal ${glslopt_sources})
target_link_libraries(glsl2agal glsl_optimizer)
