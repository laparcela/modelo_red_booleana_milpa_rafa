#!/bin/bash
#$ -N cb_manejos
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/cb_desyer.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/cb_desyerPlagui.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/cb_herb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/cb_plaguiHerb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/cb_Roundup.R
