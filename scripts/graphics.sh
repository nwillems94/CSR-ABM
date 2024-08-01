#!/bin/bash
#SBATCH -J flareGra
#SBATCH -o LOG_%j.out
#SBATCH -p skx
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 00:29:00
#SBATCH --mail-type=all
#SBATCH --mail-user=?
#SBATCH -d 
#export MKL_NUM_THREADS=48
nruns=32

export PATH=$WORK/apps/4.0.3/bin/:$PATH
JOBID_0=""

cd $WORK/CSR-ABM/
Rscript pub_graphics.R "refID"=$JOBID_0 &
wait 

Rscript flaringABM_graphics.R "refID"=$JOBID_0 & 
wait

exit 0 
