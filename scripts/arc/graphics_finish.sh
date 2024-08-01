#!/bin/bash
#SBATCH -J flareGph
#SBATCH -o LOG_%j.out
#SBATCH -p skx-normal
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 01:59:00
#SBATCH -A *
#SBATCH --mail-type=all
#SBATCH --mail-user=*
#export MKL_NUM_THREADS=96

module load Rstats
export PATH=/home/pandoc/bin:$PATH

# reference directory
cd $SCRATCH/flaringABM_10629585

file=`basename ./outputs/processed/agent_states_* .csv.gz`
JOBID_0=${file/agent_states_/}

mkdir -p graphics

Rscript flaringABM_graphics.R "refID"=$JOBID_0

echo -e "\nCompleted grapihcs: ${JOBID_0}" 

# copy processed outputs to WORK directory
xz -c ./outputs/processed/all_states_${JOBID_0}.sqlite >\
    $WORK/flaringABM/outputs/processed/all_states_${JOBID_0}.sqlite.xz 

exit 0 

