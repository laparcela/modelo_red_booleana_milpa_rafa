#!/bin/bash
#$ -N mzfre_desyerPlagui_arvenses_1212
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/arvenses/mzfre_desyerPlagui_arvenses_1212.R
