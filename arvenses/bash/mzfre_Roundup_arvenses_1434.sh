#!/bin/bash
#$ -N mzfre_Roundup_arvenses_1434
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/arvenses/mzfre_Roundup_arvenses_1434.R
