cmake_minimum_required(VERSION 3.25)
project(gpu_offload_tests VERSION 1.0 LANGUAGES Fortran)

include(CTest)
find_package(OpenACC COMPONENTS Fortran REQUIRED)

add_subdirectory(openacc)
#add_subdirectory(openmp)
