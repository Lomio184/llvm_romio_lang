cmake_minimum_required(VERSION 3.17.0)

PROJECT(RealRomio)

set(LLVM_INFRA "/Users/hazard/CLANG_API")

find_package(Boost COMPONENTS filesystem REQUIRED)

include_directories(${/Users/hazard/Boost/libs})
set( Boost_USE_STATIC_LIBS OFF )
set( Boost_USE_MULTITHREADED ON )
set( Boost_USE_STATIC_RUNTIME OFF)

find_library(LLVM_INFRA clang-c clang llvm llvm-c)

link_directories(${LLVM_INFRA}/lib)
include_directories(${LLVM_INFRA}/include)


set(CMAKE_CXX_STANDARD 14)
add_compile_options(-fno-rtti)
add_executable(RealRomio main.cpp)

target_link_libraries(
        RealRomio PRIVATE
        LLVMAsmParser
        LLVMIRReader
        LLVMCore
        LLVMCodeGen
        LLVMLinker
        clangAST
        clangAnalysis
        clangDriver
        clangFrontend
        clangLex
        clangParse
        clangIndex
)


