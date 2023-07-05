GPU offload unit-tests
**********************

This repository contains a series of unit-tests to test standard compliance of compiler toolchains for pragma-based GPU offload directives for Fortran. Tests are included for both OpenACC and OpenMP.


Building
--------
Building the unit-test suite is as simple as:
```
cmake -B build
cmake --build build
```

Running test-suite
------------------
For both OpenACC and OpenMP, there are two categories of tests:
 - 'arguments': Data is passed down to device-code as subroutine arguments
 - 'globalvars': Data is imported into device-code as module variables

The tests can therefore be filtered accordingly:
```
ctest --test-dir build [-R <openacc/openmp> <arg/globalvar>]
```

OpenACC deepcopy
----------------
This suite also contains tests to verify the functionality of the OpenACC `-gpu=deepcopy` flag. These tests can be enabled at build-time:
```
cmake -B build -DDEEPCOPY=ON
```
