This code base should work with the following compilers:
- g++ 5.3.0
- clang++ 3.4.1
- clang++ 3.6.2

Avoid the g++ 4.x branch. It does not follow SFINAE rules correctly.

If you want to build something, e.g. the tests just run make like this:
make CXXFLAGS=-std=c++11 src/tests/units_Units

Most tests are performed during compilation, but there are a couple of
runtime tests, so you may want to execute the file.
