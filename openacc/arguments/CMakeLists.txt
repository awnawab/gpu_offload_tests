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
    add_executable(${TEST_NAME}.x ${TEST_FILE})
    target_link_libraries(${TEST_NAME}.x OpenACC::OpenACC_Fortran)
    add_test(NAME openacc_${TEST_NAME}_arg COMMAND ${TEST_NAME}.x)
endforeach()
