#!/bin/bash
#$ -N mz_plaguiHerb_herbivoros_110910
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/herbivoros/mz_plaguiHerb_herbivoros_110910.R
