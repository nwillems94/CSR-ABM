#!/bin/bash
#SBATCH -J flareThreshS
#SBATCH -o LOG_%j.out
#SBATCH -p skx
#SBATCH -n 1
#SBATCH -N 1
#SBATCH -t 03:29:00
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
echo -e "\nStarting threshold sensitivity"

{ echo "01";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.01  >> ./logs/run_log_${SLURM_JOBID}_1.txt; } & 

{ sleep 61; # pause to ensure unique jobIDs
echo "02";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.02  >> ./logs/run_log_${SLURM_JOBID}_2.txt; } & 

{ sleep 121; # pause to ensure unique jobIDs
echo "03";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.03  >> ./logs/run_log_${SLURM_JOBID}_3.txt; } & 

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
Rscript flaringABM_postproc.R --parallel=16 "refID"=$JOBID_0 "T01"=$JOBID_1 "T02"=$JOBID_2 "T03"=$JOBID_3; } & 

{ echo "04";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.04  >> ./logs/run_log_${SLURM_JOBID}_4.txt; } & 

{ sleep 61; # pause to ensure unique jobIDs
echo "06";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.06  >> ./logs/run_log_${SLURM_JOBID}_5.txt; } & 

wait

echo -e "\nPost processing" 

# extract jobIDs
JOBID_4=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_4.txt | cut -d " " -f 2` 
JOBID_5=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_5.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_4.txt ./logs/run_log_${JOBID_4}.txt 
mv ./logs/run_log_${SLURM_JOBID}_5.txt ./logs/run_log_${JOBID_5}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_4 & concat_runs "market" $JOBID_4 & concat_runs "agent" $JOBID_4 & concat_runs "lease" $JOBID_4 & 
concat_runs "log" $JOBID_5 & concat_runs "market" $JOBID_5 & concat_runs "agent" $JOBID_5 & concat_runs "lease" $JOBID_5 & 
wait 

# process outputs into singular compact files
{ echo -e "\nPost processing: ${JOBID_4}, ${JOBID_5}";
Rscript flaringABM_postproc.R --parallel=16 "refID"=$JOBID_0 "T04"=$JOBID_4 "T06"=$JOBID_5; } & 

{ echo "07";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.07  >> ./logs/run_log_${SLURM_JOBID}_6.txt; } & 

{ sleep 61; # pause to ensure unique jobIDs
echo "08";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.08  >> ./logs/run_log_${SLURM_JOBID}_7.txt; } & 

wait

echo -e "\nPost processing" 

# extract jobIDs
JOBID_6=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_6.txt | cut -d " " -f 2` 
JOBID_7=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_7.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_6.txt ./logs/run_log_${JOBID_6}.txt 
mv ./logs/run_log_${SLURM_JOBID}_7.txt ./logs/run_log_${JOBID_7}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_6 & concat_runs "market" $JOBID_6 & concat_runs "agent" $JOBID_6 & concat_runs "lease" $JOBID_6 & 
concat_runs "log" $JOBID_7 & concat_runs "market" $JOBID_7 & concat_runs "agent" $JOBID_7 & concat_runs "lease" $JOBID_7 & 
wait 

# process outputs into singular compact files
{ echo -e "\nPost processing: ${JOBID_6}, ${JOBID_7}";
Rscript flaringABM_postproc.R --parallel=16 "refID"=$JOBID_0 "T07"=$JOBID_6 "T08"=$JOBID_7; } & 

{ echo "09";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.09  >> ./logs/run_log_${SLURM_JOBID}_8.txt; } & 

{ sleep 61; # pause to ensure unique jobIDs
echo "1";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.1  >> ./logs/run_log_${SLURM_JOBID}_9.txt; } & 

wait

echo -e "\nPost processing" 

# extract jobIDs
JOBID_8=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_8.txt | cut -d " " -f 2` 
JOBID_9=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_9.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_8.txt ./logs/run_log_${JOBID_8}.txt 
mv ./logs/run_log_${SLURM_JOBID}_9.txt ./logs/run_log_${JOBID_9}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_8 & concat_runs "market" $JOBID_8 & concat_runs "agent" $JOBID_8 & concat_runs "lease" $JOBID_8 & 
concat_runs "log" $JOBID_9 & concat_runs "market" $JOBID_9 & concat_runs "agent" $JOBID_9 & concat_runs "lease" $JOBID_9 & 
wait 

{ echo -e "\nPost processing: ${JOBID_8}, ${JOBID_9}";
Rscript flaringABM_postproc.R --parallel=16 "refID"=$JOBID_0 "T09"=$JOBID_8 "T1"=$JOBID_9; } & 

{ echo "11";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.11  >> ./logs/run_log_${SLURM_JOBID}_11.txt; } & 

{ sleep 61; # pause to ensure unique jobIDs
echo "12";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.12  >> ./logs/run_log_${SLURM_JOBID}_12.txt; } & 

wait

echo -e "\nPost processing" 

# extract jobIDs
JOBID_11=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_11.txt | cut -d " " -f 2` 
JOBID_12=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_12.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_11.txt ./logs/run_log_${JOBID_11}.txt 
mv ./logs/run_log_${SLURM_JOBID}_12.txt ./logs/run_log_${JOBID_12}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_11 & concat_runs "market" $JOBID_11 & concat_runs "agent" $JOBID_11 & concat_runs "lease" $JOBID_11 & 
concat_runs "log" $JOBID_12 & concat_runs "market" $JOBID_12 & concat_runs "agent" $JOBID_12 & concat_runs "lease" $JOBID_12 & 
wait 

{ echo -e "\nPost processing: ${JOBID_11}, ${JOBID_12}";
Rscript flaringABM_postproc.R --parallel=16 "refID"=$JOBID_0 "T11"=$JOBID_11 "T12"=$JOBID_12; } & 

{ echo "13";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.13  >> ./logs/run_log_${SLURM_JOBID}_13.txt; } & 

{ sleep 61; # pause to ensure unique jobIDs
echo "14";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.14  >> ./logs/run_log_${SLURM_JOBID}_14.txt; } & 

wait

echo -e "\nPost processing" 

# extract jobIDs
JOBID_13=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_13.txt | cut -d " " -f 2` 
JOBID_14=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_14.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_13.txt ./logs/run_log_${JOBID_13}.txt 
mv ./logs/run_log_${SLURM_JOBID}_14.txt ./logs/run_log_${JOBID_14}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_13 & concat_runs "market" $JOBID_13 & concat_runs "agent" $JOBID_13 & concat_runs "lease" $JOBID_13 & 
concat_runs "log" $JOBID_14 & concat_runs "market" $JOBID_14 & concat_runs "agent" $JOBID_14 & concat_runs "lease" $JOBID_14 & 
wait 

{ echo -e "\nPost processing: ${JOBID_13}, ${JOBID_14}";
Rscript flaringABM_postproc.R --parallel=16 "refID"=$JOBID_0 "T13"=$JOBID_13 "T14"=$JOBID_14; } & 

{ echo "15";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.15  >> ./logs/run_log_${SLURM_JOBID}_15.txt; } & 

{ sleep 61; # pause to ensure unique jobIDs
echo "16";
Rscript flaringABM_exe.R --parallel=16 nruns=$nruns refID=$JOBID_0 t0=0 threshold=0.16  >> ./logs/run_log_${SLURM_JOBID}_16.txt; } & 

wait

echo -e "\nPost processing" 

# extract jobIDs
JOBID_15=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_15.txt | cut -d " " -f 2` 
JOBID_16=`grep "jobID:" ./logs/run_log_${SLURM_JOBID}_16.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${SLURM_JOBID}_15.txt ./logs/run_log_${JOBID_15}.txt 
mv ./logs/run_log_${SLURM_JOBID}_16.txt ./logs/run_log_${JOBID_16}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_15 & concat_runs "market" $JOBID_15 & concat_runs "agent" $JOBID_15 & concat_runs "lease" $JOBID_15 & 
concat_runs "log" $JOBID_16 & concat_runs "market" $JOBID_16 & concat_runs "agent" $JOBID_16 & concat_runs "lease" $JOBID_16 & 
wait 

{ echo -e "\nPost processing: ${JOBID_15}, ${JOBID_16}";
Rscript flaringABM_postproc.R "refID"=$JOBID_0 "T15"=$JOBID_15 "T16"=$JOBID_16; } & 
wait

echo -e "\nCompleted jobs: ${JOBID_1}, ${JOBID_2}, ${JOBID_3}, ${JOBID_4}, ${JOBID_5}, ${JOBID_6}, ${JOBID_7}, ${JOBID_8}, ${JOBID_9}, ${JOBID_11}, ${JOBID_12}, ${JOBID_13}, ${JOBID_14}, ${JOBID_15}, ${JOBID_16}" 

exit 0 
