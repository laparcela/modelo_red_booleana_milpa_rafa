#!/bin/bash
#$ -N mz_Roundup_arvenses_1767
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/arvenses/mz_Roundup_arvenses_1767.R
