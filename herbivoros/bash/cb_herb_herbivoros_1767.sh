#!/bin/bash
#$ -N cb_herb_herbivoros_1767
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/herbivoros/cb_herb_herbivoros_1767.R
