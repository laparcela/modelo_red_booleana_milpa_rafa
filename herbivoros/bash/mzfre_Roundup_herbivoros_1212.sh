#!/bin/bash
#$ -N mzfre_Roundup_herbivoros_1212
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/herbivoros/mzfre_Roundup_herbivoros_1212.R
