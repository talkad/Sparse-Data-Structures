#!/bin/sh

gfortran -O$1 -fopenmp -o exe demo_algorithms.f90 main.f90

rm *.o
rm *.mod

