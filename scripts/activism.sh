#!/bin/bash
#SBATCH -J flareAct
#SBATCH -o LOG_%j.out
#SBATCH -p skx
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 01:29:00
#SBATCH --mail-type=all
#SBATCH --mail-user=?
#SBATCH -d 
#export MKL_NUM_THREADS=48
nruns=32

export PATH=$WORK/apps/4.0.3/bin/:$PATH
JOBID_0=""

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


# stage code and data
cd $SCRATCH

echo "Working directory is: $SCRATCH/flaringABM_${SLURM_JOBID}"

mkdir -p flaringABM_${SLURM_JOBID}/inputs/processed 
mkdir -p flaringABM_${SLURM_JOBID}/outputs/processed 
mkdir flaringABM_${SLURM_JOBID}/outputs/validation 
mkdir flaringABM_${SLURM_JOBID}/logs 

# copy code
cp $WORK/CSR-ABM/*.R* flaringABM_${SLURM_JOBID}/
# copy input data
cp $WORK/CSR-ABM/inputs/processed/* flaringABM_${SLURM_JOBID}/inputs/processed/
# copy reference outputs
cp $WORK/CSR-ABM/outputs/*_${JOBID_0}* flaringABM_${SLURM_JOBID}/outputs/
cp $WORK/CSR-ABM/logs/param_log_${JOBID_0}* flaringABM_${SLURM_JOBID}/logs/

cd flaringABM_${SLURM_JOBID}/

# start comparitive runs simultaneously
echo -e "\nStarting activist runs"

# even targetting
{ echo "targetting evenly";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 strategy='even' >> ./logs/run_log_${SLURM_JOBID}_1.txt; } & 

# target top flarers
{ sleep 61; # pause to ensure unique jobIDs
echo "targetting top";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 strategy='top' >> ./logs/run_log_${SLURM_JOBID}_2.txt; } & 

# shock event
{ sleep 121; # pause to ensure unique jobIDs
echo "shock event";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 strategy='shock' >> ./logs/run_log_${SLURM_JOBID}_3.txt; } & 

wait

echo -e "\nPost processing" 

# extract jobIDs
JOBID_1=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_1.txt | cut -d " " -f 2` 
JOBID_2=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_2.txt | cut -d " " -f 2` 
JOBID_3=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_3.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_1.txt ./logs/run_log_${JOBID_1}.txt 
mv ./logs/run_log_${SLURM_JOBID}_2.txt ./logs/run_log_${JOBID_2}.txt 
mv ./logs/run_log_${SLURM_JOBID}_3.txt ./logs/run_log_${JOBID_3}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_1 & concat_runs "market" $JOBID_1 & concat_runs "agent" $JOBID_1 & concat_runs "lease" $JOBID_1 & 
concat_runs "log" $JOBID_2 & concat_runs "market" $JOBID_2 & concat_runs "agent" $JOBID_2 & concat_runs "lease" $JOBID_2 & 
concat_runs "log" $JOBID_3 & concat_runs "market" $JOBID_3 & concat_runs "agent" $JOBID_3 & concat_runs "lease" $JOBID_3 & 
wait 

# process outputs into singular compact files
{ echo -e "\nPost processing: ${JOBID_1}, ${JOBID_2}, ${JOBID_3}";
Rscript flaringABM_postproc.R "refID"=$JOBID_0 "target even"=$JOBID_1 "target top"=$JOBID_2 "shock event"=$JOBID_3; } & 

wait

echo -e "\nCompleted jobs: ${JOBID_0}, ${JOBID_1}, ${JOBID_2}, ${JOBID_3}" 

exit 0 
