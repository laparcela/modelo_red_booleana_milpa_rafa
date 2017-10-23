#!/bin/bash
#$ -N mz_manejos
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mz_desyer.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mz_desyerPlagui.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mz_herb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mz_plaguiHerb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mz_Roundup.R
