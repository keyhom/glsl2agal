cmake_minimum_required(VERSION 2.8)

project(glsl2agal)

include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_SOURCE_DIR}/src/mesa)
include_directories(${PROJECT_SOURCE_DIR}/src/mapi)
include_directories(${PROJECT_SOURCE_DIR}/src/glsl)
include_directories(${PROJECT_SOURCE_DIR}/src)

option (ENABLE_TEST "Enable testing" OFF)
option (USE_BUILTIN_FEATURES "Use the GCC/Clang builtin features specific by GNU toolchain." OFF)

if(AVM2PlusCC OR FLASCC)
    option (AVM2CC_LLVM_LTO "Enable link time optimization for executable." OFF)

    if (AVM2PlusCC)
        option (EMIT_SWC "Enable to generate a SWC library in AVM2PlusCC." ON)
        option (EMIT_SWE "Enable to emit swe as default in AVM2PlusCC." ON)
        set(AVM2CC_LLVM_LTO_LEVEL 3 CACHE INTEGER "Emit link time optimization level for executable." FORCE)
    endif()
endif()

if(NOT CMAKE_BUILD_TYPE)
    if(AVM2PlusCC OR FLASCC)
        set(CMAKE_BUILD_TYPE "MinSizeRel" CACHE STRING "Choose the type to Configuration types build." FORCE)
    else()
        set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Choose the type to Configuration types build." FORCE)
    endif()
endif()

if(FLASCC)
    add_definitions(-emit-llvm)
endif()

if(USE_BUILTIN_FEATURES)
    if("${CMAKE_C_COMPILER_ID}" MATCHES Clang OR "${CMAKE_C_COMPILER_ID}" MATCHES GNU)
        add_definitions(-DHAVE___BUILTIN_CLZ)
        add_definitions(-DHAVE___BUILTIN_CLZLL)
        add_definitions(-DHAVE___BUILTIN_FFS)
        add_definitions(-DHAVE___BUILTIN_FFSLL)
        add_definitions(-DHAVE___BUILTIN_POPCOUNT)
        add_definitions(-DHAVE___BUILTIN_POPCOUNTLL)
        add_definitions(-DHAVE___BUILTIN_EXPECT)
        add_definitions(-DHAVE___BUILTIN_UNREACHABLE)
    endif()
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
if(MSVC OR MSVC_IDE)
    list(APPEND glsl_sources ${PROJECT_SOURCE_DIR}/src/getopt/getopt_long.c)
    include_directories(${PROJECT_SOURCE_DIR}/src/getopt)
endif()
list(REMOVE_ITEM glsl_sources ${glsl_sources_remove})
add_library(glsl_optimizer ${glsl_sources})
target_link_libraries(glsl_optimizer glcpp-library mesa agalasm)

add_executable(glsl_compiler ${PROJECT_SOURCE_DIR}/src/glsl/main.cpp)
target_link_libraries(glsl_compiler glsl_optimizer)

if(ENABLE_TEST)
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
list(REMOVE_ITEM glslopt_sources ${PROJECT_SOURCE_DIR}/contrib/glslopt/Main.cpp)
add_executable(glslopt ${glslopt_sources})
target_link_libraries(glslopt glsl_optimizer)

#add_executable(glcpp src/glsl/glcpp/glcpp.c)
#target_link_libraries(glcpp glsl_optimizer)

if(AVM2PlusCC OR FLASCC)
    add_custom_target(agal_optimizer
        COMMAND cd ${PROJECT_SOURCE_DIR}/agaloptimiser/src && bash genabc.sh
        COMMAND mv ${PROJECT_SOURCE_DIR}/agaloptimiser/src/agaloptimiser.abc ${PROJECT_BINARY_DIR}/agaloptimiser.abc
        WORKING_DIRECTORY ${PROJECT_BINARY_DIR}
        COMMENT "Built AGAL optimizer, ABC generated."
        )

    add_dependencies(glslopt agal_optimizer)
    target_link_libraries(glslopt ${PROJECT_BINARY_DIR}/agaloptimiser.abc)

    set(LINKER_FLAGS)
    if(AVM2CC_LLVM_LTO)
        set(LINKER_FLAGS "--llvm-lto ${AVM2CC_LLVM_LTO_LEVEL}")
    elseif(FLASCC)
        set(LINKER_FLAGS "-flto-api=exports.txt")
    endif()

    if(EMIT_SWE)
        set_target_properties(glsl_compiler
            PROPERTIES
            SUFFIX ".swe"
            LINK_FLAGS "${LINKER_FLAGS}"
            )
        set_target_properties(glslopt
            PROPERTIES
            SUFFIX ".swe"
            LINK_FLAGS "${LINKER_FLAGS}"
            )
    endif()

    if(EMIT_SWC)
        set(LINKER_FLAGS "${LINKER_FLAGS} -emit-swc=com.adobe.glsl2agal")
        add_executable(glslopt_swc ${glslopt_sources})
        add_dependencies(glslopt_swc agal_optimizer)
        target_link_libraries(glslopt_swc glsl_optimizer ${PROJECT_BINARY_DIR}/agaloptimiser.abc)
        set_target_properties(glslopt_swc
            PROPERTIES
            COMPILE_DEFINITIONS "SWC"
            OUTPUT_NAME "glsl2agal"
            SUFFIX ".swc"
            LINK_FLAGS "${LINKER_FLAGS}"
            )
    endif()
endif()

