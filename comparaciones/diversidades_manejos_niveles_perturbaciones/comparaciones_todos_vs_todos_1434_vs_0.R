source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_todos_vs_todos.R")
load("~/Dropbox/Chido/permanencias.RData")
a<-permanencias.perturbacion

perturbacion=c("det","precipitacion","arvenses","herbivoros")
#manejo=c("desyer")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
#manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb")
nivel=c("1212", "1434", "1434", "110910")
#nivel=c("0","1212", "1434", "1434", "1434")
dimen=(length(perturbacion)*length(manejo)*length(diversidad))
matrizMZ<-matrix(0,dimen,dimen)
matrizFR<-matrix(0,dimen,dimen)
matrizCB<-matrix(0,dimen,dimen)
matrizQ<-matrix(0,dimen,dimen)
matrizSH<-matrix(0,dimen,dimen)
alfa=1-(1-0.05)^(1/(dimen-1))
zeta=qnorm(1-(alfa/2))
#numero=0
for(j in 1:length(diversidad)){
	for(i in 1:length(manejo)){
		for(h in 1:length(perturbacion)){
			if(h==1) {nivel=c("0"); level=1}
			if(h!=1) {nivel=c("1434"); level=2}
			for(k in level){	
				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in 1:length(perturbacion)){
							if(l==1) {nivel=c("0"); level=1}
							if(l!=1) {nivel=c("1434"); level=2}
							for(o in level){
#								numero=numero+1
#								print(numero)
								b<-(a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[o]]$MzG_MzJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$MzG_MzJ[2]/100+a[[l]][[m]][[n]][[o]]$MzG_MzJ[2]/100)
#								print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$MzG_MzJ[2]!=0 & a[[l]][[m]][[n]][[o]]$MzG_MzJ[2]!=0){
									if(b>zeta | b<(-zeta)){
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[o]]$MzG_MzJ[1]
									}else{
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
									}
								}else{
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
								}
							}
						}
					}
				}
			}
		}
	}
}
for(j in 1:length(diversidad)){
	for(i in 1:length(manejo)){
		for(h in 1:length(perturbacion)){
			if(h==1) {nivel=c("0"); level=1}
			if(h!=1) {nivel=c("1434"); level=2}
			for(k in level){
				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in 1:length(perturbacion)){
							if(l==1) {nivel=c("0"); level=1}
							if(l!=1) {nivel=c("1434"); level=2}
							for(o in level){
#								numero=numero+1
#								print(numero)
								b<-(a[[h]][[i]][[j]][[k]]$FreG_Fre[1]-a[[l]][[m]][[n]][[o]]$FreG_Fre[1])/sqrt(a[[h]][[i]][[j]][[k]]$FreG_Fre[2]/100+a[[l]][[m]][[n]][[o]]$FreG_Fre[2]/100)
#								print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$FreG_Fre[2]!=0 & a[[l]][[m]][[n]][[o]]$FreG_Fre[2]!=0){
									if(b>zeta | b<(-zeta)){
										matrizFR[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$FreG_Fre[1]-a[[l]][[m]][[n]][[o]]$FreG_Fre[1]
									}else{
										matrizFR[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
									}
								}else{
										matrizFR[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
								}
							}
						}
					}
				}
			}
		}
	}
}
for(j in 1:length(diversidad)){
	for(i in 1:length(manejo)){
		for(h in 1:length(perturbacion)){
			if(h==1) {nivel=c("0"); level=1}
			if(h!=1) {nivel=c("1434"); level=2}
			for(k in level){
				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in 1:length(perturbacion)){
							if(l==1) {nivel=c("0"); level=1}
							if(l!=1) {nivel=c("1434"); level=2}
							for(o in level){
#								numero=numero+1
#								print(numero)
								b<-(a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[o]]$CbG_CbJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$CbG_CbJ[2]/100+a[[l]][[m]][[n]][[o]]$CbG_CbJ[2]/100)
#								print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$CbG_CbJ[2]!=0 & a[[l]][[m]][[n]][[o]]$CbG_CbJ[2]!=0){
									if(b>zeta | b<(-zeta)){
										matrizCB[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[o]]$CbG_CbJ[1]
									}else{
										matrizCB[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
									}
								}else{
										matrizCB[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
								}
							}
						}
					}
				}
			}
		}
	}
}
for(j in 1:length(diversidad)){
	for(i in 1:length(manejo)){
		for(h in 1:length(perturbacion)){
			if(h==1) {nivel=c("0"); level=1}
			if(h!=1) {nivel=c("1434"); level=2}
			for(k in level){
				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in 1:length(perturbacion)){
							if(l==1) {nivel=c("0"); level=1}
							if(l!=1) {nivel=c("1434"); level=2}
							for(o in level){
#								numero=numero+1
#								print(numero)
								b<-(a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[l]][[m]][[n]][[o]]$Quelites[1])/sqrt(a[[h]][[i]][[j]][[k]]$Quelites[2]/100+a[[l]][[m]][[n]][[o]]$Quelites[2]/100)
#								print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$Quelites[2]!=0 & a[[l]][[m]][[n]][[o]]$Quelites[2]!=0){
									if(b>zeta | b<(-zeta)){
										matrizQ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[l]][[m]][[n]][[o]]$Quelites[1]
									}else{
										matrizQ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
									}
								}else{
										matrizQ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
								}
							}
						}
					}
				}
			}
		}
	}
}
for(j in 1:length(diversidad)){
	for(i in 1:length(manejo)){
		for(h in 1:length(perturbacion)){
			if(h==1) {nivel=c("0"); level=1}
			if(h!=1) {nivel=c("1434"); level=2}
			for(k in level){
				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in 1:length(perturbacion)){
							if(l==1) {nivel=c("0"); level=1}
							if(l!=1) {nivel=c("1434"); level=2}
							for(o in level){
#								numero=numero+1
#								print(numero)
								b<-(a[[h]][[i]][[j]][[k]]$conj[1]-a[[l]][[m]][[n]][[o]]$conj[1])/sqrt(a[[h]][[i]][[j]][[k]]$conj[2]/100+a[[l]][[m]][[n]][[o]]$conj[2]/100)
#								print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$conj[2]!=0 & a[[l]][[m]][[n]][[o]]$conj[2]!=0){
									if(b>zeta | b<(-zeta)){
										matrizSH[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$conj[1]-a[[l]][[m]][[n]][[o]]$conj[1]
									}else{
										matrizSH[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
									}
								}else{
										matrizSH[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
								}
							}
						}
					}
				}
			}
		}
	}
}
pdf("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/todos_vs_todos_1434_vs_0.pdf",height=10, width=16)
myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz\n Todas las perturbaciones, nivel=1/4, control determinista",manejo="otro")
myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol\n Todas las perturbaciones, nivel=1/4, control determinista",manejo="otro")
myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza\n Todas las perturbaciones, nivel=1/4, control determinista",manejo="otro")
myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites\n Todas las perturbaciones, nivel=1/4, control determinista",manejo="otro")
myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta\n Todas las perturbaciones, nivel=1/4, control determinista",manejo="otro")
dev.off()
cerosMZ<-which(apply(matrizMZ,1,sum)==0)
matrizMZ<-abs(matrizMZ[-cerosMZ,-cerosMZ])
cerosFR<-which(apply(matrizFR,1,sum)==0)
matrizFR<-abs(matrizFR[-cerosFR,-cerosFR])
cerosCB<-which(apply(matrizCB,1,sum)==0)
matrizCB<-abs(matrizCB[-cerosCB,-cerosCB])
#cerosQ<-which(apply(matrizQ,1,sum)==0)
matrizQ<-abs(matrizQ)#[-cerosQ,-cerosQ])
#cerosSH<-which(apply(matrizSH,1,sum)==0)
matrizSH<-abs(matrizSH)#[-cerosSH,-cerosSH]
pdf("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/sentido_vs_sentido_1434_vs_0.pdf",height=10, width=16)
source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_filtrado_mz.R")
myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz \n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_filtrado_fre.R")
myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol\n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_filtrado_cb.R")
myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza\n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_todos_vs_todos.R")
myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites\n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta\n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
dev.off()
