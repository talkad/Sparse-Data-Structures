#!/bin/sh

gfortran -O0 -fopenmp -o exe data.f90 dynamic_array.f90 mat_array.f90 algorithm.f90 main.f90

rm *.o
rm *.mod

