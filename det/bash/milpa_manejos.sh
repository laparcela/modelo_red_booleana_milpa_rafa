#!/bin/bash
#$ -N milpa_manejos
#$ -M camahui21@gmail.com 
#$ -cwd
#$ -S /bin/bash
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/milpa_desyer.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/milpa_desyerPlagui.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/milpa_herb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/milpa_plaguiHerb.R
module load R/3.1.3
Rscript ~/Dropbox/Chido/det/milpa_Roundup.R
