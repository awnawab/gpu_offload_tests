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
