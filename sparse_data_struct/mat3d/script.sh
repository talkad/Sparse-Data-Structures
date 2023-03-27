#!/bin/sh

gfortran -O$1 -fopenmp -o exe data.f90 mat3d.f90 algorithm.f90 main.f90

rm *.o
rm *.mod

