source("~/Dropbox/Chido/plotImage_todos_vs_todos_110910_vs_0.R")
load("~/Dropbox/Chido/permanencias.RData")
a<-permanencias.perturbacion

perturbacion=c("det","precipitacion","arvenses","herbivoros")
#manejo=c("desyer")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
#manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb")
nivel=c("1212", "1434", "1767", "110910")
#nivel=c("0","1212", "1434", "1767", "110910")
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
			if(h!=1) {nivel=c("110910"); level=4}
			for(k in level){
				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in 1:length(perturbacion)){
							if(l==1) {nivel=c("0"); level=1}
							if(l!=1) {nivel=c("110910"); level=4}
							for(o in level){
#								numero=numero+1
#								print(numero)
								b<-(a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[o]]$MzG_MzJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$MzG_MzJ[2]+a[[l]][[m]][[n]][[o]]$MzG_MzJ[2])
#								print(b)
								if(b!="NaN"){
									if(b>zeta | b<(-zeta)){
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(i-1)*length(nivel)+length(nivel)*(h-1)+1, length(manejo)*(n-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(m-1)*length(nivel)+length(nivel)*(l-1)+1]<-a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[o]]$MzG_MzJ[1]
									}else{
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(i-1)*length(nivel)+length(nivel)*(h-1)+1, length(manejo)*(n-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(m-1)*length(nivel)+length(nivel)*(l-1)+1]<-0
									}
								}else{
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(i-1)*length(nivel)+length(nivel)*(h-1)+1, length(manejo)*(n-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(m-1)*length(nivel)+length(nivel)*(l-1)+1]<-0
								}
							}
						}
					}
				}
			}
		}
	}
}
#for(j in 1:length(diversidad)){
#	for(i in 1:length(manejo)){
#		for(h in 1:length(perturbacion)){
#			for(k in 1:length(nivel)){
#				for(n in 1:length(diversidad)){
#					for(m in 1:length(manejo)){
#						for(l in 1:length(perturbacion)){
#							for(o in 1:length(nivel)){
#								#numero=numero+1
#								#print(numero)
#								b<-(a[[h]][[i]][[j]][[k]]$FreG_Fre[1]-a[[l]][[m]][[n]][[o]]$FreG_Fre[1])/sqrt(a[[h]][[i]][[j]][[k]]$FreG_Fre[2]+a[[l]][[m]][[n]][[o]]$FreG_Fre[2])
#								if(b!="NaN"){
#									if(b>zeta | b<(-zeta)){
#										matrizFR[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$FreG_Fre[1]-a[[l]][[m]][[n]][[o]]$FreG_Fre[1]
#									}else{
#										matrizFR[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
#									}
#								}else{
#									matrizFR[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
#								}
#							}
#						}
#					}
#				}
#			}
#		}
#	}
#}
#for(j in 1:length(diversidad)){
#	for(i in 1:length(manejo)){
#		for(h in 1:length(perturbacion)){
#			for(k in 1:length(nivel)){
#				for(n in 1:length(diversidad)){
#					for(m in 1:length(manejo)){
#						for(l in 1:length(perturbacion)){
#							for(o in 1:length(nivel)){
#								#numero=numero+1
#								#print(numero)
#								b<-(a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[o]]$CbG_CbJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$CbG_CbJ[2]+a[[l]][[m]][[n]][[o]]$CbG_CbJ[2])
#								if(b!="NaN"){
#									if(b>zeta | b<(-zeta)){
#										matrizCB[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[o]]$CbG_CbJ[1]
#									}else{
#										matrizCB[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
#									}
#								}else{
#									matrizCB[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
#								}
#							}
#						}
#					}
#				}
#			}
#		}
#	}
#}
#for(j in 1:length(diversidad)){
#	for(i in 1:length(manejo)){
#		for(h in 1:length(perturbacion)){
#			for(k in 1:length(nivel)){
#				for(n in 1:length(diversidad)){
#					for(m in 1:length(manejo)){
#						for(l in 1:length(perturbacion)){
#							for(o in 1:length(nivel)){
#								#numero=numero+1
#								#print(numero)
#								b<-(a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[l]][[m]][[n]][[o]]$Quelites[1])/sqrt(a[[h]][[i]][[j]][[k]]$Quelites[2]+a[[l]][[m]][[n]][[o]]$Quelites[2])
#								if(b!="NaN"){
#									if(b>zeta | b<(-zeta)){
#										matrizQ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[l]][[m]][[n]][[o]]$Quelites[1]
#									}else{
#										matrizQ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
#									}
#								}else{
#									matrizQ[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
#								}
#							}
#						}
#					}
#				}
#			}
#		}
#	}
#}
#for(j in 1:length(diversidad)){
#	for(i in 1:length(manejo)){
#		for(h in 1:length(perturbacion)){
#			for(k in 1:length(nivel)){
#				for(n in 1:length(diversidad)){
#					for(m in 1:length(manejo)){
#						for(l in 1:length(perturbacion)){
#							for(o in 1:length(nivel)){
#								#numero=numero+1
#								#print(numero)
#								b<-(a[[h]][[i]][[j]][[k]]$conj[1]-a[[l]][[m]][[n]][[o]]$conj[1])/sqrt(a[[h]][[i]][[j]][[k]]$conj[2]+a[[l]][[m]][[n]][[o]]$conj[2])
#								if(b!="NaN"){
#									if(b>zeta | b<(-zeta)){
#										matrizSH[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$conj[1]-a[[l]][[m]][[n]][[o]]$conj[1]
#									}else{
#										matrizSH[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
#									}
#								}else{
#									matrizSH[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-0
#								}
#							}
#						}
#					}
#				}
#			}
#		}
#	}
#}
pdf("resultados2.pdf",height=10, width=16)
myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz",manejo="otro")
#myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol",manejo="otro")
#myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza",manejo="otro")
#myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites",manejo="otro")
#myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta",manejo="otro")
dev.off()
