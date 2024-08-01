#!/bin/bash

SLURM1=$(echo `sbatch start.sh` | awk '{print $NF}')
# SLURM1=$(echo `sbatch restart.sh` | awk '{print $NF}')

echo $SLURM1

# auto-populate reference ID in other scripts
for script in reporting.sh social.sh market.sh activism.sh graphics.sh; do
  sed -i "/JOBID_0=.*/c\JOBID_0=\"$SLURM1\"" $script
done

## Queue reporting scenarios
# set dependency
sed -i "/#SBATCH -d .*/c\#SBATCH -d $SLURM1" ./reporting.sh
SLURM2=$(echo `sbatch reporting.sh` | awk '{print $NF}')
echo $SLURM2


## Queue on-off scenarios
# set dependency
sed -i "/#SBATCH -d .*/c\#SBATCH -d $SLURM2" ./social.sh
SLURM3=$(echo `sbatch social.sh` | awk '{print $NF}')
echo $SLURM3


## Queue queue market scenarios
# set dependency
sed -i "/#SBATCH -d .*/c\#SBATCH -d $SLURM3" ./market.sh
SLURM4=$(echo `sbatch market.sh` | awk '{print $NF}')
echo $SLURM4


## Queue activism scenarios
# set dependency
sed -i "/#SBATCH -d .*/c\#SBATCH -d $SLURM4" ./activism.sh
SLURM5=$(echo `sbatch activism.sh` | awk '{print $NF}')
echo $SLURM5


cd ./sensitivity/
for script in activism.sh size.sh threshold.sh; do
  sed -i "/JOBID_0=.*/c\JOBID_0=\"$SLURM1\"" $script
done

## Queue activism sensitivity
# set dependency
sed -i "/#SBATCH -d .*/c\#SBATCH -d $SLURM5" ./activism.sh
SLURM6=$(echo `sbatch activism.sh` | awk '{print $NF}')
echo $SLURM6

## Queue market size sensitivity
# set dependency
sed -i "/#SBATCH -d .*/c\#SBATCH -d $SLURM6" ./size.sh
SLURM7=$(echo `sbatch size.sh` | awk '{print $NF}')
echo $SLURM7

## Queue market size sensitivity
# set dependency
sed -i "/#SBATCH -d .*/c\#SBATCH -d $SLURM7" ./threshold.sh
SLURM8=$(echo `sbatch threshold.sh` | awk '{print $NF}')
echo $SLURM8


## Queue graphics
cd ..
# set dependency
sed -i "/#SBATCH -d .*/c\#SBATCH -d $SLURM8" ./graphics.sh
SLURM9=$(echo `sbatch graphics.sh` | awk '{print $NF}')
echo $SLURM9


exit 0
