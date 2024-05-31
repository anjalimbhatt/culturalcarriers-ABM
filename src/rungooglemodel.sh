#!/bin/bash
#
#BSUB -q short # Queue to submit to
#BSUB -n 16 # Number of cores
#BSUB -W 24:00 # Runtime in [HH:]MM
#BSUB -R "rusage[mem=5G]" # Memory reservation for the job
#BSUB -M 15G -hl # Upper memory limit
#BSUB -o ./log/googlemodel_%J.out # File to which STDOUT will be written
#BSUB -B -N # Send email when job begins & ends/fails
#BSUB -u ambhatt.hbs.edu

module load rcs/rcs_2023.09
Rscript ./src/culturalcarriers_fixedinitial_googlemodel_202404.R