#!/bin/bash
#$ -N mzcb_manejos
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzcb_desyer.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzcb_desyerPlagui.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzcb_herb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzcb_plaguiHerb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzcb_Roundup.R
