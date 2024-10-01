#######################################################################################################################
System Tests
#######################################################################################################################

Introduction
############
A flexible test mechanism has been developed for iWrap to easily check its functional correctness. A number of
test cases are currently available to verify the correctness of code wrapping and actor generation.
The tests are designed to minimize maintenance effort by avoiding code redundancy, keeping
as much of the code shared and reusable as possible.
Most of the test cases are created to wrap codes written in various languages (C++, Fortran, Java)
and handle various code parameter formats (XML, JSON, the Fortran Namelist), and compiled using two different
compilers (GCC and Intel) so the number of actual tests that can be run exceeds 100.

The following chapters describe:

* How to run the tests and analyze their results?
* How to add a new test case?
* How to configure the test environment?
