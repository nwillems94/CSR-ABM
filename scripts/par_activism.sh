#!/bin/bash
#SBATCH -J flareAct
#SBATCH -o LOG_%j.out
#SBATCH -p skx-normal
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 00:59:00
#SBATCH -A *
#SBATCH --mail-type=all
#SBATCH --mail-user=*
#export MKL_NUM_THREADS=96
nruns=32

module load Rstats


# concatenate runs into single file and delete components
function concat_runs {
    if [ $1 == "log" ]; then
        awk '(NR == 1) || (FNR > 1)' `eval "ls -v ./logs/param_log_$2-{1..$nruns}.csv"` > ./logs/param_log_$2.csv || break 
        eval "rm ./logs/param_log_$2-{1..$nruns}.csv"
        eval "cat ./logs/run_log_$2-{1..$nruns}.txt >> ./logs/run_log_$2.txt"
        eval "rm  ./logs/run_log_$2-{1..$nruns}.txt"
    else
        awk '(NR == 1) || (FNR > 1)' `eval "ls -v ./outputs/$1_states_$2-{1..$nruns}.csv"` > ./outputs/$1_states_$2.csv || break 
        eval "rm ./outputs/$1_states_$2-{1..$nruns}.csv"
    fi
}

# reference directory
cd $SCRATCH/flaringABM_10629585

file=`basename ./outputs/processed/agent_states_* .csv.gz`
JOBID_0=${file/agent_states_/}

# start comparitive runs simultaneously 
echo -e "\nStarting alternative runs"

# activist strategy 1
{ echo "targetting evenly";
Rscript flaringABM_exe.R --parallel=48 nruns=$nruns refID=$JOBID_0 t0=0 strategy='even' >> ./logs/run_log_${SLURM_JOBID}_1.txt; } &

# activist strategy 2
{ sleep 60; # pause to ensure unique jobIDs 
echo "targetting top";
Rscript flaringABM_exe.R --parallel=48 nruns=$nruns refID=$JOBID_0 t0=0 strategy='top' >> ./logs/run_log_${SLURM_JOBID}_2.txt; } &

wait


echo -e "\nPost processing" 

# extract jobIDs
JOBID_1=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_1.txt | cut -d " " -f 2` 
JOBID_2=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_2.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_1.txt ./logs/run_log_${JOBID_1}.txt 
mv ./logs/run_log_${SLURM_JOBID}_2.txt ./logs/run_log_${JOBID_2}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_1 & concat_runs "market" $JOBID_1 & concat_runs "agent" $JOBID_1 & concat_runs "lease" $JOBID_1 & 
concat_runs "log" $JOBID_2 & concat_runs "market" $JOBID_2 & concat_runs "agent" $JOBID_2 & concat_runs "lease" $JOBID_2 & 
wait 


# process outputs into singular compact files 
Rscript flaringABM_postproc.R "refID"=$JOBID_0 "target even"=$JOBID_1 "target top"=$JOBID_2


echo -e "\nCompleted jobs: ${JOBID_1}, ${JOBID_2}" 

exit 0 

