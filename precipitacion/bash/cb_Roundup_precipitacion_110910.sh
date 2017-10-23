#!/bin/bash
#$ -N cb_Roundup_precipitacion_110910
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/precipitacion/cb_Roundup_precipitacion_110910.R
