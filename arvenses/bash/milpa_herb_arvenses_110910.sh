#!/bin/bash
#$ -N milpa_herb_arvenses_110910
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/arvenses/milpa_herb_arvenses_110910.R
