#!/bin/sh

gfortran -O0 -fopenmp -o exe data.f90 mat3d.f90 algorithm.f90 main.f90

rm *.o
rm *.mod

