#!/bin/bash
#$ -N mzcb_desyer_precipitacion_1434
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/precipitacion/mzcb_desyer_precipitacion_1434.R
