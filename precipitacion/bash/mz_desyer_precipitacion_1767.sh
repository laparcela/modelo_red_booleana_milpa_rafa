#!/bin/bash
#$ -N mz_desyer_precipitacion_1767
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/precipitacion/mz_desyer_precipitacion_1767.R
