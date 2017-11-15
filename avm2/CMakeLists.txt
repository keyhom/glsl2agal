cmake_minimum_required(VERSION 2.8)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src/mesa)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src/mapi)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src)

file(GLOB agalasm_sources ${CMAKE_CURRENT_SOURCE_DIR}/../agalassembler/*.cpp)
file(GLOB glcpp-library_sources ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/glcpp/*.c ${CMAKE_CURRENT_SOURCE_DIR}/../src/util/*.c)
file(GLOB mesa_sources ${CMAKE_CURRENT_SOURCE_DIR}/../src/mesa/program/*.c ${CMAKE_CURRENT_SOURCE_DIR}/../src/mesa/main/*.c)
file(GLOB glsl_sources ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/*.cpp ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/*.c)
file(GLOB glsl_sources_remove ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/main.cpp ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/builtin_stubs.cpp)
if(MSVC OR MSVC_IDE)
    list(APPEND glsl_sources ${CMAKE_CURRENT_SOURCE_DIR}/../src/getopt/getopt_long.c)
    include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src/getopt)
endif()
list(REMOVE_ITEM glsl_sources ${glsl_sources_remove})

add_custom_target(agal_optimizer
    COMMAND cd ${CMAKE_CURRENT_SOURCE_DIR}/../agaloptimiser/src && bash genabc.sh
    COMMAND mv ${CMAKE_CURRENT_SOURCE_DIR}/../agaloptimiser/src/agaloptimiser.abc ${CMAKE_CURRENT_BINARY_DIR}/agaloptimiser.abc.o
    WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
    COMMENT "Built AGAL optimizer, ABC generated."
    )

add_library(agal_compiler AGALCompiler.cpp ${glsl_sources} ${glcpp-library_sources} ${mesa_sources} ${agalasm_sources})
add_dependencies(agal_compiler agal_optimizer)
target_link_libraries(agal_compiler ${CMAKE_CURRENT_BINARY_DIR}/agaloptimiser.abc.o)

