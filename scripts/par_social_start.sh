#!/bin/bash
#SBATCH -J flareSoc
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


# concatenate runs into single file and delete components
function concat_runs {
    if [ $1 == "log" ]; then
        awk '(NR == 1) || (FNR > 1)' `ls -v ./logs/param_log_$2-*` > ./logs/param_log_$2.csv || break 
        rm ./logs/param_log_$2-* 
        cat ./logs/run_log_$2-{1..32}.txt >> ./logs/run_log_$2.txt
        rm  ./logs/run_log_$2-{1..32}.txt
    else
        awk '(NR == 1) || (FNR > 1)' `ls -v ./outputs/$1_states_$2-*` > ./outputs/$1_states_$2.csv || break 
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
cp $WORK/flaringABM/* flaringABM_${SLURM_JOBID}/ 
# copy input data
cp $WORK/flaringABM/inputs/processed/* flaringABM_${SLURM_JOBID}/inputs/processed/ 

cd flaringABM_${SLURM_JOBID}/ 


# run models
echo "running baseline model"
Rscript flaringABM_exe.R >> ./logs/run_log_${SLURM_JOBID}_0.txt || exit 

# get jobID for reference in following runs
JOBID_0=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_0.txt | cut -d " " -f 2` 

# rename run log appropriately
mv ./logs/run_log_${SLURM_JOBID}_0.txt ./logs/run_log_${JOBID_0}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_0 & concat_runs "market" $JOBID_0 & concat_runs "agent" $JOBID_0 & concat_runs "lease" $JOBID_0 & 
wait 


echo "  ...allowing time to sync files..." 
sleep 120 


# start comparitive runs simultaneously 
echo -e "\nStarting comparative runs"

# no imitation
{ echo "running without imitation ... "; 
Rscript flaringABM_exe.R --parallel=32 refID=$JOBID_0 prob_m=0 t0=0 >> ./logs/run_log_${SLURM_JOBID}_1.txt; } & 

# no differentiation
{ sleep 60; # pause to ensure unique jobIDs 
echo "running without differentiation ... "; 
Rscript flaringABM_exe.R --parallel=32 refID=$JOBID_0 market_prop_green=0 t0=0 >> ./logs/run_log_${SLURM_JOBID}_2.txt; } & 

{ sleep 65; 
echo -e "\nPost processing: ${JOBID_0}"; 
Rscript flaringABM_postproc.R --parallel=32 "complete"=$JOBID_0; } & 

wait

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

# no stakeholder activism
{ echo "running without activism ... "; 
Rscript flaringABM_exe.R --parallel=32 refID=$JOBID_0 Activism=0 t0=0 >> ./logs/run_log_${SLURM_JOBID}_3.txt; } & 

# no shareholder valuation
{ sleep 60; # pause to ensure unique jobIDs 
echo "running without shareholder valuation ... "; 
Rscript flaringABM_exe.R --parallel=32 refID=$JOBID_0 SRoR=0 t0=0 >> ./logs/run_log_${SLURM_JOBID}_4.txt; } & 

{ sleep 65; 
echo -e "\nPost processing: ${JOBID_1}, ${JOBID_2}";
Rscript flaringABM_postproc.R --parallel=32 "refID"=$JOBID_0 "no imitation"=$JOBID_1 "no differentiation"=$JOBID_2; } & 

wait

# extract jobIDs
JOBID_3=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_3.txt | cut -d " " -f 2` 
JOBID_4=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_4.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_3.txt ./logs/run_log_${JOBID_3}.txt 
mv ./logs/run_log_${SLURM_JOBID}_4.txt ./logs/run_log_${JOBID_4}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_3 & concat_runs "market" $JOBID_3 & concat_runs "agent" $JOBID_3 & concat_runs "lease" $JOBID_3 & 
concat_runs "log" $JOBID_4 & concat_runs "market" $JOBID_4 & concat_runs "agent" $JOBID_4 & concat_runs "lease" $JOBID_4 & 
wait 


# process outputs into singular compact files 
{ echo -e "\nPost processing: ${JOBID_3}";
Rscript flaringABM_postproc.R --parallel=48 "refID"=$JOBID_0 "no stakeholder\nactivism"=$JOBID_3; } & 

{ sleep 600; # pause to avoid concurrent db requests
echo -e "\nPost processing: ${JOBID_4}";
Rscript flaringABM_postproc.R --parallel=48 "refID"=$JOBID_0 "no shareholder\nvaluation"=$JOBID_4; } & 

wait


Rscript flaringABM_verification.R "complete"=$JOBID_0


echo -e "\nCompleted jobs: ${JOBID_0}, ${JOBID_1}, ${JOBID_2}, ${JOBID_3}, ${JOBID_4}" 

exit 0 

