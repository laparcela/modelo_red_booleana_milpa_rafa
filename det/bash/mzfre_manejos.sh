#!/bin/bash
#$ -N mzfre_manejos
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzfre_desyer.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzfre_desyerPlagui.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzfre_herb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzfre_plaguiHerb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/mzfre_Roundup.R
