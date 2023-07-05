cmake_minimum_required(VERSION 3.25)
project(gpu_offload_tests VERSION 1.0 LANGUAGES Fortran)

include(CTest)
find_package(OpenACC COMPONENTS Fortran REQUIRED)

option(DEEPCOPY "Test -gpu=deepcopy flag for simpler derived-type offload" OFF)

add_subdirectory(openacc)
#add_subdirectory(openmp)
