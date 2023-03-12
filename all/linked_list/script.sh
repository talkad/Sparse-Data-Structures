#!/bin/sh

gfortran -O0 -fopenmp -o exe data.f90 link.f90 linked_list.f90 mat_list.f90 algorithm.f90 main.f90

rm *.o
rm *.mod

