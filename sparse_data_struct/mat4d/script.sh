#!/bin/sh

gfortran -O$1 -fopenmp -o exe sparse_struct_base.f90 mat4d.f90 demo_algorithms.f90 main.f90

rm *.o
rm *.mod

