#!/bin/bash
#SBATCH -J flareRep
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


# concatenate runs into single file and delete components
function concat_runs {
    if [ $1 == "log" ]; then
        awk '(NR == 1) || (FNR > 1)' `ls -v ./logs/param_log_$2-*` > ./logs/param_log_$2.csv || break 
        rm ./logs/param_log_$2-* 
        cat ./logs/run_log_$2-{1..32}.txt >> ./logs/run_log_$2.txt
        rm  ./logs/run_log_$2-{1..32}.txt
    else
        awk '(NR == 1) || (FNR > 1)' `ls -v ./outputs/$1_states_$2-*` > ./outputs/$1_states_$2.csv || break 
        rm ./outputs/$1_states_$2-* 
    fi
}

# reference directory
cd $SCRATCH/flaringABM_10629585

file=`basename ./outputs/processed/agent_states_* .csv.gz`
JOBID_0=${file/agent_states_/}

# start comparitive runs simultaneously 
echo -e "\nStarting alternative market scenarios" 


# assume flared casinghead gas is misreported
{ echo "running with mis-reported casinghead gas";
Rscript flaringABM_exe.R --parallel=48 refID=$JOBID_0 reporting='misreported' >> ./logs/run_log_${SLURM_JOBID}_1.txt; } &

# assume flared casinghead gas is underreported
{ sleep 60; # pause to ensure unique jobIDs
echo "running with under-reported casinghead gas";
Rscript flaringABM_exe.R --parallel=48 refID=$JOBID_0 reporting='underreported' >> ./logs/run_log_${SLURM_JOBID}_2.txt; } &

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
Rscript flaringABM_postproc.R "refID"=$JOBID_0 "mis-reported"=$JOBID_1 "under-reported"=$JOBID_2


echo -e "\nCompleted jobs: ${JOBID_1}, ${JOBID_2}" 

exit 0 
