#!/bin/sh

gfortran -O1 -fopenmp -o exe data.f90 sparse_matrix.f90 algorithm.f90 main.f90

rm *.o
rm *.mod

