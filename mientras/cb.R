source("~/Dropbox/Chido/plotImage.R")
load("~/Dropbox/Chido/permanencias_.RData")
a<-permanencias.perturbacion

perturbacion=c("precipitacion","arvenses","herbivoros","det")
#perturbacion=c("det","det", "det", "det")
#manejo=c("desyer")
manejo=c("desyer", "desyerPlagui")
#, "herb", "plaguiHerb", "Roundup")
#manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa","mzcb","cb")
#diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb")
nivel=c("1212", "1434", "1767", "110910")
#nivel=c("0","1212", "1434", "1767", "110910")
dimen=(length(perturbacion)*length(manejo)*length(diversidad)*length(nivel))
matrizMZ<-matrix(0,dimen,dimen)
matrizFR<-matrix(0,dimen,dimen)
matrizCB<-matrix(0,dimen,dimen)
matrizQ<-matrix(0,dimen,dimen)
matrizSH<-matrix(0,dimen,dimen)
alfa=1-(1-0.05)^(1/(length(perturbacion)*length(manejo)*length(diversidad)*length(nivel)-1))
zeta=qnorm(1-(alfa/2))
#numero=0
for(j in 1:length(diversidad)){
	for(i in 1:length(manejo)){
		for(h in 1:length(perturbacion)){
			if(h==4) nivel=c("0")
			if(h!=4) nivel=c("1212", "1434", "1767", "110910")
			for(k in 1:length(nivel)){
				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in 1:length(perturbacion)){
							if(l==4) nivel=c("0")
							if(l!=4) nivel=c("1212", "1434", "1767", "110910")
							for(o in 1:length(nivel)){
								#numero=numero+1
								#print(numero)
								b<-(a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[o]]$CbG_CbJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$CbG_CbJ[2]+a[[l]][[m]][[n]][[o]]$CbG_CbJ[2])
								if(b!="NaN"){
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
#								b<-(a[[h]][[i]][[j]][[k]]$Shannon[1]-a[[l]][[m]][[n]][[o]]$Shannon[1])/sqrt(a[[h]][[i]][[j]][[k]]$Shannon[2]+a[[l]][[m]][[n]][[o]]$Shannon[2])
#								if(b!="NaN"){
#									if(b>zeta | b<(-zeta)){
#										matrizSH[length(manejo)*(j-1)*length(perturbacion)+length(perturbacion)*(i-1)+h, length(manejo)*(n-1)*length(perturbacion)+length(perturbacion)*(m-1)+l]<-a[[h]][[i]][[j]][[k]]$Shannon[1]-a[[l]][[m]][[n]][[o]]$Shannon[1]
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
pdf("resultados.pdf",height=10, width=16)
myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz",manejo="otro")
#myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol",manejo="otro")
#myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza",manejo="otro")
#myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites",manejo="otro")
#myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta (Shannon)",manejo="otro")
dev.off()
