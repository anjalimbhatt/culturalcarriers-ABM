#!/bin/bash
#
#BSUB -q short # Queue to submit to
#BSUB -n 16 # Number of cores
#BSUB -R "rusage[mem=5G]" # Memory reservation for the job
#BSUB -M 15G -hl # Upper memory limit
#BSUB -o ./log/baselinemodel_%J.out # File to which STDOUT will be written
#BSUB -B -N # Send email when job begins & ends/fails

module load R/4.0.2
Rscript ./src/culturalcarriers_fixedinitial_baselinemodel_202404.R