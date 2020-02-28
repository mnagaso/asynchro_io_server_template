#!/bin/sh

make clean
make

#mpirun.mpich -np 6 ./sample_io_server
mpirun -np 6 ./sample_io_server
