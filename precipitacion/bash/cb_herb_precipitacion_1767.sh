#!/bin/bash
#$ -N cb_herb_precipitacion_1767
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/precipitacion/cb_herb_precipitacion_1767.R
