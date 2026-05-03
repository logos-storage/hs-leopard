#!/bin/bash

g++ -O3 -std=c++11 -c *.cpp
ar rcs libleopard.a *.o