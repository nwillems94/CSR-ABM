#!/bin/bash
#SBATCH -J flareStr
#SBATCH -o LOG_%j.out
#SBATCH -p skx
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 01:29:00
#SBATCH --mail-type=all
#SBATCH --mail-user=?
#export MKL_NUM_THREADS=48
nruns=32

export PATH=$WORK/apps/4.0.3/bin/:$WORK/apps/pandoc-2.11/bin/:$PATH
JOBID_0=""
JOBID_0b=${JOBID_0}b

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

cd $WORK/CSR-ABM/
# unpack db
xz -dc ./outputs/processed/all_states_${JOBID_0}.sqlite.xz >\
    ./outputs/processed/all_states_${JOBID_0b}.sqlite

R -q -e "source('flaringABM_core.R'); recover_outputs('$JOBID_0b')"

# stage code and data
cd $SCRATCH

echo "Working directory is: $SCRATCH/flaringABM_${SLURM_JOBID}"

mkdir -p flaringABM_${SLURM_JOBID}/inputs/processed 
mkdir -p flaringABM_${SLURM_JOBID}/outputs/processed 
mkdir flaringABM_${SLURM_JOBID}/outputs/validation 
mkdir flaringABM_${SLURM_JOBID}/logs 
# copy reference outputs
cp $WORK/CSR-ABM/outputs/*_${JOBID_0b}* flaringABM_${SLURM_JOBID}/outputs/
cp $WORK/CSR-ABM/logs/param_log_${JOBID_0b}* flaringABM_${SLURM_JOBID}/logs/

# copy code
cp $WORK/CSR-ABM/*.R* flaringABM_${SLURM_JOBID}/
# copy input data
cp $WORK/CSR-ABM/inputs/processed/* flaringABM_${SLURM_JOBID}/inputs/processed/

cd flaringABM_${SLURM_JOBID}/


# re run complete
{ echo "rerunning complete ... "; 
Rscript flaringABM_exe.R --parallel=48 nruns=$nruns refID=$JOBID_0b >> ./logs/run_log_${SLURM_JOBID}_1.txt; } & 
wait

echo -e "\nPost processing" 

# extract jobIDs
JOBID_1=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_1.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_1.txt ./logs/run_log_${JOBID_1}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_1 & concat_runs "market" $JOBID_1 & concat_runs "agent" $JOBID_1 & concat_runs "lease" $JOBID_1 & 
wait 


{ echo -e "\nPost processing: ${JOBID_1}";
Rscript flaringABM_postproc.R "refID"=$JOBID_0b "complete2"=$JOBID_1; } & 
wait

R -q -e "source('flaringABM_core.R'); reset_db('$JOBID_0b')"

# rename file and recreate CSVs needed for inputs
cd $WORK/CSR-ABM/
rm $WORK/CSR-ABM/outputs/*_${JOBID_0b}*
rm $WORK/CSR-ABM/logs/param_log_${JOBID_0b}*

mv ./outputs/processed/all_states_${JOBID_0b}.sqlite\
    ./outputs/processed/all_states_${SLURM_JOBID}.sqlite

R -q -e "source('flaringABM_core.R'); recover_outputs('$SLURM_JOBID')"

Rscript flaringABM_verification.R "refID"=$SLURM_JOBID


exit 0 
