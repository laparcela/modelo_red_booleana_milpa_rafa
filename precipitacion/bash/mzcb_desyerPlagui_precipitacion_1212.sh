#!/bin/bash
#$ -N mzcb_desyerPlagui_precipitacion_1212
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/precipitacion/mzcb_desyerPlagui_precipitacion_1212.R
