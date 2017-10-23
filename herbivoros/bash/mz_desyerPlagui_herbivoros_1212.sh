#!/bin/bash
#$ -N mz_desyerPlagui_herbivoros_1212
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/herbivoros/mz_desyerPlagui_herbivoros_1212.R
