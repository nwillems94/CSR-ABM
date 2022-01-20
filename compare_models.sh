#!/bin/bash

# add R and pandoc to path
export PATH=/c/Users/nwill/Software/MRO/R-4.0.2/bin/x64/:/c/Users/nwill/Software/pandoc/pandoc-2.14.2:$PATH
UNIQUEID="temp"$[RANDOM%10000+10000]

# concatenate runs into single file and remove precursors
function concat_runs {
    if [ $1 == "log" ]; then
        awk '(NR == 1) || (FNR > 1)' `ls -v ./logs/param_log_$2-*` > ./logs/param_log_$2.csv || break 
        rm ./logs/param_log_$2-* 
    else
        awk '(NR == 1) || (FNR > 1)' `ls -v ./outputs/$1_states_$2-*` > ./outputs/$1_states_$2.csv || break 
        rm ./outputs/$1_states_$2-* 
    fi
}

echo "running baseline model"
RScript flaringABM_exe.R >> ./logs/run_log_${UNIQUEID}_0.txt || exit

# get jobID for reference in following runs
JOBID_0=`grep "jobID:" ./logs/run_log_${UNIQUEID}_0.txt | cut -d " " -f 2`
# rename run log appropriately
mv ./logs/run_log_${UNIQUEID}_0.txt ./logs/run_log_${JOBID_0}.txt

# concatenate run outputs
concat_runs "log" $JOBID_0 & 
concat_runs "agent" $JOBID_0 & 
concat_runs "lease" $JOBID_0 & 
wait 

echo "  ...giving Box time to sync files..."
sleep 300


echo -e "\nStarting comparative runs"

# no imitation
{ echo "running without imitation"; 
RScript flaringABM_exe.R refID=$JOBID_0 prob_m=0 >> ./logs/run_log_${UNIQUEID}_1.txt; } &

# no differentiation
{ sleep 60; # pause to ensure unique jobIDs 
echo "running without differentiation"; 
RScript flaringABM_exe.R refID=$JOBID_0 market_prop_green=0 >> ./logs/run_log_${UNIQUEID}_2.txt; } &

# no stakeholder activism
{ sleep 120; # pause to ensure unique jobIDs 
echo "running without activism"; 
RScript flaringABM_exe.R refID=$JOBID_0 Activism=0 >> ./logs/run_log_${UNIQUEID}_3.txt; } &

wait


echo -e "\nPost processing"

# extract jobIDs
JOBID_1=`grep "jobID:" ./logs/run_log_${UNIQUEID}_1.txt | cut -d " " -f 2` 
JOBID_2=`grep "jobID:" ./logs/run_log_${UNIQUEID}_2.txt | cut -d " " -f 2` 
JOBID_3=`grep "jobID:" ./logs/run_log_${UNIQUEID}_3.txt | cut -d " " -f 2` 

# rename run logs appropriately
mv ./logs/run_log_${UNIQUEID}_1.txt ./logs/run_log_${JOBID_1}.txt 
mv ./logs/run_log_${UNIQUEID}_2.txt ./logs/run_log_${JOBID_2}.txt 
mv ./logs/run_log_${UNIQUEID}_3.txt ./logs/run_log_${JOBID_3}.txt 

# concatenate run outputs
concat_runs "log" $JOBID_1 & 
concat_runs "agent" $JOBID_1 & 
concat_runs "lease" $JOBID_1 & 
concat_runs "log" $JOBID_2 & 
concat_runs "agent" $JOBID_2 & 
concat_runs "lease" $JOBID_2 & 
concat_runs "log" $JOBID_3 & 
concat_runs "agent" $JOBID_3 & 
concat_runs "lease" $JOBID_3 & 
wait 


echo -e "\nCompleted jobs: ${JOBID_0}, ${JOBID_1}, ${JOBID_2}, ${JOBID_3}" 
exit 0
