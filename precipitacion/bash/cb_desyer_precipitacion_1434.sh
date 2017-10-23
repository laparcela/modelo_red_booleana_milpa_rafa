#!/bin/bash
#$ -N cb_desyer_precipitacion_1434
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/precipitacion/cb_desyer_precipitacion_1434.R
