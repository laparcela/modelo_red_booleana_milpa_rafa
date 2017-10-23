#!/bin/bash
#$ -N mzcb_plaguiHerb_arvenses_1767
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/arvenses/mzcb_plaguiHerb_arvenses_1767.R
