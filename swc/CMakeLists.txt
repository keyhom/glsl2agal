cmake_minimum_required(VERSION 2.8)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../include)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src/mesa)
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fno-exceptions -fno-rtti -O4 -DNDEBUG -DUNIX")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -fno-exceptions -fno-rtti -O4 -DNDEBUG -DUNIX")
option (SDK "Path to the Alchemy2 SDK" "/path/to/alchemy2/sdk")

file(GLOB agalasm_sources ${CMAKE_CURRENT_SOURCE_DIR}/../agalassembler/*.cpp)
file(GLOB glcpp-library_sources ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/glcpp/*.c)
file(GLOB glcpp-library_sources_remove ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/glcpp/glcpp.c)
list(REMOVE_ITEM glcpp-library_sources ${glcpp-library_sources_remove})
file(GLOB mesa_sources ${CMAKE_CURRENT_SOURCE_DIR}/../src/mesa/program/*.c)
file(GLOB glsl_sources ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/*.cpp ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/*.c)
file(GLOB glsl_sources_remove ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/main.cpp ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/builtin_stubs.cpp ${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl/swc.cpp)
list(REMOVE_ITEM glsl_sources ${glsl_sources_remove})

add_library(glsl2agal ${agalasm_sources} ${glcpp-library_sources} ${mesa_sources} ${glsl_sources})

add_custom_target(agal_optimizer
    COMMAND cd ${CMAKE_CURRENT_SOURCE_DIR}/../agaloptimiser/src && bash genabc.sh
    COMMAND cd ${CMAKE_CURRENT_BINARY_DIR} && mv ${CMAKE_CURRENT_SOURCE_DIR}/../agaloptimiser/src/agaloptimiser.abc agaloptimiser.abc
    )

add_custom_target(swc ALL COMMAND ${CMAKE_CXX_COMPILER} -fno-exceptions -fno-rtti -O4 -flto-api=exports.txt -emit-swc=com.adobe.glsl2agal -o glsl2agal.swc agaloptimiser.abc ${CMAKE_CURRENT_SOURCE_DIR}/swc.cpp libglsl2agal.a -I${CMAKE_CURRENT_SOURCE_DIR}/../include -I${CMAKE_CURRENT_SOURCE_DIR}/../src/mesa -I${CMAKE_CURRENT_SOURCE_DIR}/../src/mapi -I${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl DEPENDS glsl2agal agal_optimizer)

add_custom_target(glsl2agalopt ALL COMMAND ${CMAKE_CXX_COMPILER} -DCMDLINE=1 -fno-exceptions -fno-rtti -O4 -flto-api=exports.txt -o glsl2agalopt agaloptimiser.abc ${CMAKE_CURRENT_SOURCE_DIR}/swc.cpp libglsl2agal.a -I${CMAKE_CURRENT_SOURCE_DIR}/../include -I${CMAKE_CURRENT_SOURCE_DIR}/../src/mesa -I${CMAKE_CURRENT_SOURCE_DIR}/../src/mapi -I${CMAKE_CURRENT_SOURCE_DIR}/../src/glsl DEPENDS glsl2agal agal_optimizer)

