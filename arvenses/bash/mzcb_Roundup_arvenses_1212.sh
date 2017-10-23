#!/bin/bash
#$ -N mzcb_Roundup_arvenses_1212
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/arvenses/mzcb_Roundup_arvenses_1212.R
