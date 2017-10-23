###############################
### Declarando tratamientos ###
###############################

# Perturbaciones 
dir0<-c("det", "precipitacion", "arvenses", "herbivoros")
# Niveles de perturbación
pert<-c("1212", "1343", "1767", "110910")
# Esquemas de manejo
dir1<-c("desyer", "desyerPlagui", "Herb", "plaguiHerb", "Roundup")
# Esquemas de riqueza
dir2<-c("milpa", "mzfre", "mzcb", "mz", "cb")

#########################################
### Generando dataset de permanencias ###
#########################################
for(i in 1:length(dir2)){
	for(j in 1:length(dir1)){
		for(k in 1:length(dir0)){
			for(l in 1:length(pert)){
				load(paste0(dir2[i],"_",dir1[j],"_",dir0[k],"_",pert[l]".RData"))
				lista.repe
				# Function for extracting permanences
			}
		}
	}
}

