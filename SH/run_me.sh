#!/bin/bash
CORES=8
for i in `seq $CORES`
do 
echo -ne "OMP_NUM_THREADS=" $i
export OMP_NUM_THREADS=$i 
./ym_cpu|grep SH  
done 
