#!/bin/bash
#$ -N cb_Roundup_herbivoros_1767
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/herbivoros/cb_Roundup_herbivoros_1767.R
