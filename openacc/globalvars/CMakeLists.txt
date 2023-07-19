# (C) Copyright 2023- ECMWF.
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

set(TEST_FILES
    static_arrays.F90
    allocatable_arrays.F90
    derived_type_static.F90
    derived_type_allocatable.F90
    derived_type_nested_static.F90
    derived_type_nested_allocatable.F90
)

foreach(TEST_FILE ${TEST_FILES})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    set(CMAKE_Fortran_MODULE_DIRECTORY CMAKE_BINARY_DIR/module/openacc_${TEST_NAME}_globalvar)
    add_executable(openacc_${TEST_NAME}_globalvar.x ${TEST_FILE})
    target_link_libraries(openacc_${TEST_NAME}_globalvar.x OpenACC::OpenACC_Fortran)
    add_test(NAME openacc_${TEST_NAME}_globalvar COMMAND openacc_${TEST_NAME}_globalvar.x)
endforeach()

if( DEEPCOPY )
    set(TEST_NAME "derived_type_allocatable_deepcopy")
    set(TEST_FILE ${TEST_NAME})
    string(APPEND TEST_FILE ".F90")
    set(CMAKE_Fortran_MODULE_DIRECTORY CMAKE_BINARY_DIR/module/openacc_${TEST_NAME}_globalvar)
    add_executable(openacc_${TEST_NAME}_globalvar.x ${TEST_FILE} vars_mod.F90)
    target_link_libraries(openacc_${TEST_NAME}_globalvar.x OpenACC::OpenACC_Fortran)
    set_source_files_properties(${TEST_FILE} PROPERTIES COMPILE_OPTIONS "-gpu=deepcopy")
    add_test(NAME openacc_${TEST_NAME}_globalvar COMMAND openacc_${TEST_NAME}_globalvar.x)

    set(TEST_NAME "derived_type_nested_allocatable_deepcopy")
    set(TEST_FILE ${TEST_NAME})
    string(APPEND TEST_FILE ".F90")
    set(CMAKE_Fortran_MODULE_DIRECTORY CMAKE_BINARY_DIR/module/openacc_${TEST_NAME}_globalvar)
    add_executable(openacc_${TEST_NAME}_globalvar.x ${TEST_FILE} vars_mod_nested.F90)
    target_link_libraries(openacc_${TEST_NAME}_globalvar.x OpenACC::OpenACC_Fortran)
    set_source_files_properties(${TEST_FILE} PROPERTIES COMPILE_OPTIONS "-gpu=deepcopy")
    add_test(NAME openacc_${TEST_NAME}_globalvar COMMAND openacc_${TEST_NAME}_globalvar.x)
endif()
