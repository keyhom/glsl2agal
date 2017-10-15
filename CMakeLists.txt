cmake_minimum_required(VERSION 2.8)

project(glsl2agal)

include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_SOURCE_DIR}/src/mesa)
include_directories(${PROJECT_SOURCE_DIR}/src/mapi)
include_directories(${PROJECT_SOURCE_DIR}/src/glsl)
include_directories(${PROJECT_SOURCE_DIR}/src)
option (DEBUG "Enable debugging" FALSE)
option (ENABLE_TEST "Enable testing" FALSE)

if(${DEBUG} MATCHES "on")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -g -O0")
	set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -O0")
else()
    set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Os -DNDEBUG")
    set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -Os -DNDEBUG")
    set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -s")
endif()

file(GLOB agalasm_sources ${PROJECT_SOURCE_DIR}/agalassembler/*.cpp)
add_library(agalasm ${agalasm_sources})

file(GLOB glcpp-library_sources ${PROJECT_SOURCE_DIR}/src/glsl/glcpp/*.c ${PROJECT_SOURCE_DIR}/src/util/*.c)
# file(GLOB glcpp-library_sources_remove ${PROJECT_SOURCE_DIR}/src/glsl/glcpp/glcpp.c)
# list(REMOVE_ITEM glcpp-library_sources ${glcpp-library_sources_remove})
add_library(glcpp-library ${glcpp-library_sources})

file(GLOB mesa_sources ${PROJECT_SOURCE_DIR}/src/mesa/program/*.c ${PROJECT_SOURCE_DIR}/src/mesa/main/*.c)
add_library(mesa ${mesa_sources})

file(GLOB glsl_sources ${PROJECT_SOURCE_DIR}/src/glsl/*.cpp src/glsl/*.c)
file(GLOB glsl_sources_remove ${PROJECT_SOURCE_DIR}/src/glsl/main.cpp ${PROJECT_SOURCE_DIR}/src/glsl/builtin_stubs.cpp)
list(REMOVE_ITEM glsl_sources ${glsl_sources_remove})
add_library(glsl_optimizer ${glsl_sources})
target_link_libraries(glsl_optimizer glcpp-library mesa agalasm)

add_executable(glsl_compiler ${PROJECT_SOURCE_DIR}/src/glsl/main.cpp)
target_link_libraries(glsl_compiler glsl_optimizer)

if("${ENABLE_TEST}" MATCHES "on")
    file(GLOB glsl_test_sources ${PROJECT_SOURCE_DIR}/tests/*.cpp)
    add_executable(glsl_test ${glsl_test_sources})
    target_link_libraries(glsl_test glsl_optimizer)
    if(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
        # OSX-specific build requirements
        find_library(OpenGL_LIBRARY OpenGL )
        target_link_libraries(glsl_test ${OpenGL_LIBRARY})
    endif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
endif()

file(GLOB glslopt_sources ${PROJECT_SOURCE_DIR}/contrib/glslopt/*.cpp)
add_executable(glslopt ${glslopt_sources})
target_link_libraries(glslopt glsl_optimizer)

#add_executable(glcpp src/glsl/glcpp/glcpp.c)
#target_link_libraries(glcpp glsl_optimizer)
