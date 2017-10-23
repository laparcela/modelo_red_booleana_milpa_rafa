#!/bin/bash
#$ -N mzcb_plaguiHerb_herbivoros_1212
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/herbivoros/mzcb_plaguiHerb_herbivoros_1212.R
