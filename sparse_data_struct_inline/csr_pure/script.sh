#!/bin/sh

gfortran -O$1 -Ofast -flto -fopenmp -o exe sparse_struct_base.f90 csr.f90 demo_algorithms.f90 main.f90

rm *.o
rm *.mod

