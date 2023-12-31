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
    set(CMAKE_Fortran_MODULE_DIRECTORY CMAKE_BINARY_DIR/module/openacc_${TEST_NAME}_arg)
    add_executable(openacc_${TEST_NAME}_arg.x ${TEST_FILE})
    target_link_libraries(openacc_${TEST_NAME}_arg.x OpenACC::OpenACC_Fortran)
    add_test(NAME openacc_${TEST_NAME}_arg COMMAND openacc_${TEST_NAME}_arg.x)
endforeach()

if( DEEPCOPY )
   set(TEST_FILES_DEEPCOPY
       derived_type_allocatable_deepcopy.F90
       derived_type_nested_allocatable_deepcopy.F90
   )

   foreach(TEST_FILE ${TEST_FILES_DEEPCOPY})
       get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
       set(CMAKE_Fortran_MODULE_DIRECTORY CMAKE_BINARY_DIR/module/openacc_${TEST_NAME}_arg)
       add_executable(openacc_${TEST_NAME}_arg.x ${TEST_FILE})
       target_link_libraries(openacc_${TEST_NAME}_arg.x OpenACC::OpenACC_Fortran)
       target_compile_options(openacc_${TEST_NAME}_arg.x PRIVATE "-gpu=deepcopy")
       add_test(NAME openacc_${TEST_NAME}_arg COMMAND openacc_${TEST_NAME}_arg.x)
   endforeach()
endif()
