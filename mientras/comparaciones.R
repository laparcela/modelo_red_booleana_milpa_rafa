source("~/Dropbox/Chido/plotImage.R")
load("~/Dropbox/Chido/permanencias_Shannon_det.RData")
a<-permanencias.perturbacion

perturbacion=c("precipitacion","arvenses","herbivoros","det")
#perturbacion=c("det","det", "det", "det")
#manejo=c("desyer")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
#manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
#diversidad=c("milpa","mzcb")
diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb")
nivel=c("0","1212")
#nivel=c("1212", "1434", "1767", "110910")
#nivel=c("0","1212", "1434", "1767", "110910")
dimen=(length(manejo)*length(diversidad)*length(nivel))
matrizMZ<-matrix(0,dimen,dimen)
matrizFR<-matrix(0,dimen,dimen)
matrizCB<-matrix(0,dimen,dimen)
matrizQ<-matrix(0,dimen,dimen)
matrizSH<-matrix(0,dimen,dimen)
alfa=1-(1-0.05)^(1/79)
zeta=qnorm(1-(alfa/2))

for(h in 1:length(perturbacion)){
for(i in 1:length(manejo)){
	for(j in 1:length(diversidad)){
		for(k in 1:length(nivel)){
			for(m in 1:length(manejo)){
				for(n in 1:length(diversidad)){
					for(o in 1:length(nivel)){
						b<-(a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[h]][[m]][[n]][[o]]$MzG_MzJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$MzG_MzJ[2]+a[[h]][[m]][[n]][[o]]$MzG_MzJ[2])
						if(b!="NaN"){
							if(b>zeta | b<(-zeta)){
								matrizMZ[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[h]][[m]][[n]][[o]]$MzG_MzJ[1]
							}else{
								matrizMZ[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
							}
						}else{
							matrizMZ[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
						}
					}
				}
			}
		}
	}
}
}

#for(i in 1:length(manejo)){
#	for(j in 1:length(diversidad)){
#		for(k in 1:length(nivel)){
#			for(m in 1:length(manejo)){
#				for(n in 1:length(diversidad)){
#					for(o in 1:length(nivel)){
#						b<-(a[[h]][[i]][[j]][[k]]$FreG_Fre[1]-a[[h]][[m]][[n]][[o]]$FreG_Fre[1])/sqrt(a[[h]][[i]][[j]][[k]]$FreG_Fre[2]+a[[h]][[m]][[n]][[o]]$FreG_Fre[2])
#												if(b!="NaN"){
#							if(b>zeta | b<(-zeta)){
#								matrizFR[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-a[[h]][[i]][[j]][[k]]$FreG_Fre[1]-a[[h]][[m]][[n]][[o]]$FreG_Fre[1]
#							}else{
#								matrizFR[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
#							}
#						}else{
#							matrizFR[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
#						}
#					}
#				}
#			}
#		}
#	}
#}

#for(i in 1:length(manejo)){
#	for(j in 1:length(diversidad)){
#		for(k in 1:length(nivel)){
#			for(m in 1:length(manejo)){
#				for(n in 1:length(diversidad)){
#					for(o in 1:length(nivel)){
#						b<-(a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[h]][[m]][[n]][[o]]$CbG_CbJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$CbG_CbJ[2]+a[[h]][[m]][[n]][[o]]$CbG_CbJ[2])
#												if(b!="NaN"){
#							if(b>zeta | b<(-zeta)){
#								matrizCB[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[h]][[m]][[n]][[o]]$CbG_CbJ[1]
#							}else{
#								matrizCB[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
#							}
#						}else{
#							matrizCB[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
#						}
#					}
#				}
#			}
#		}
#	}
#}

#for(i in 1:length(manejo)){
#	for(j in 1:length(diversidad)){
#		for(k in 1:length(nivel)){
#			for(m in 1:length(manejo)){
#				for(n in 1:length(diversidad)){
#					for(o in 1:length(nivel)){
#						b<-(a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[h]][[m]][[n]][[o]]$Quelites[1])/sqrt(a[[h]][[i]][[j]][[k]]$Quelites[2]+a[[h]][[m]][[n]][[o]]$Quelites[2])
#												if(b!="NaN"){
#							if(b>zeta | b<(-zeta)){
#								matrizQ[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[h]][[m]][[n]][[o]]$Quelites[1]
#							}else{
#								matrizQ[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
#							}
#						}else{
#							matrizQ[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
#						}
#					}
#				}
#			}
#		}
#	}
#}

#for(i in 1:length(manejo)){
#	for(j in 1:length(diversidad)){
#		for(k in 1:length(nivel)){
#			for(m in 1:length(manejo)){
#				for(n in 1:length(diversidad)){
#					for(o in 1:length(nivel)){
#						b<-(a[[h]][[i]][[j]][[k]]$Shannon[1]-a[[h]][[m]][[n]][[o]]$Shannon[1])/sqrt(a[[h]][[i]][[j]][[k]]$Shannon[2]+a[[h]][[m]][[n]][[o]]$Shannon[2])
#												if(b!="NaN"){
#							if(b>zeta | b<(-zeta)){
#								matrizSH[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-a[[h]][[i]][[j]][[k]]$Shannon[1]-a[[h]][[m]][[n]][[o]]$Shannon[1]
#							}else{
#								matrizSH[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
#							}
#						}else{
#							matrizSH[length(diversidad)*(i-1)*length(nivel)+length(nivel)*(j-1)+k,length(diversidad)*(m-1)*length(nivel)+length(nivel)*(n-1)+o]<-0
#						}
#					}
#				}
#			}
#		}
#	}
#}

#pdf("resultados_det.pdf",height=10, width=16)
#myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz",manejo=perturbacion)
#myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol",manejo=perturbacion)
#myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza",manejo=perturbacion)
#myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites",manejo=perturbacion)
#myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta (Shannon)",manejo=perturbacion)
#dev.off()
