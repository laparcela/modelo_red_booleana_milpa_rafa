comparacion<-function(pert,man,div,niv){
	perturbacion=c("det","precipitacion","arvenses","herbivoros")
	manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
	diversidad=c("milpa", "mzcb", "mzfre", "mz","cb")

#	if(any(pert=="det" & any)) print("va")

	indice.pert <-matrix(0,1,length(pert))
		
	for(i in 1:length(pert)){
		for(j in 1:length(perturbacion)){
			indice.pert[i]<-which(pert[i]==perturbacion[j])
		}
	}
}

comparacion(pert=c("det","precipitacion"),man=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup"),div=c("milpa", "mzcb", "mzfre", "mz","cb"),niv=c("0","110910"))
