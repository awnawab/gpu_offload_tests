# (C) Copyright 2023- ECMWF.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

cmake_minimum_required(VERSION 3.20.2)
project(gpu_offload_tests VERSION 1.0 LANGUAGES Fortran)

include(CTest)
find_package(OpenACC COMPONENTS Fortran REQUIRED)

option(DEEPCOPY "Test -gpu=deepcopy flag for simpler derived-type offload" OFF)

add_subdirectory(openacc)
#add_subdirectory(openmp)
