source("~/Dropbox/Chido/comparaciones/diversidades_manejos_perturbaciones_niveles/plotImage_todos_vs_todos.R")
load("~/Dropbox/Chido/permanencias.RData")
a<-permanencias.perturbacion

perturbacion=c("det","arvenses")
#"det","arvenses","arvenses","herbivoros")
#manejo=c("desyer")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
#manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb")
nivel=c("1212","1434","1767","110910")

dimen=(length(nivel)*length(manejo)*length(diversidad)+length(diversidad)*length(manejo))
matrizMZ<-matrix(0,dimen,dimen)
matrizFR<-matrix(0,dimen,dimen)
matrizCB<-matrix(0,dimen,dimen)
matrizQ<-matrix(0,dimen,dimen)
matrizSH<-matrix(0,dimen,dimen)
alfa=1-(1-0.05)^(1/(dimen-1))
zeta=qnorm(1-(alfa/2))

for(j in 1:length(diversidad)){
	for(i in 1:length(manejo)){
		for(h in c(1,3)){
			if(h==1) {nivel=c("0");numero=0}
			if(h!=1) {nivel=c("1212","1434","1767","110910");numero=1}
			for(k in 1:length(nivel)){
				numero=numero+1	
#			print(k)
			#print(numero)

				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in c(1,3)){
							if(l==1) {nivel=c("0");numeros=0}
							if(l!=1) {nivel=c("1212","1434","1767","110910");numeros=1}
							for(o in 1:length(nivel)){
								numeros=numeros+1
#print(o)
#							print(numeros)
								b<-(a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[o]]$MzG_MzJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$MzG_MzJ[2]/100+a[[l]][[m]][[n]][[o]]$MzG_MzJ[2]/100)
#		print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$MzG_MzJ[2]!=0 & a[[l]][[m]][[n]][[o]]$MzG_MzJ[2]!=0){
									if(b>zeta | b<(-zeta)){
										nivel=c("0","1212","1434","1767","110910")
										matrizMZ[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[o]]$MzG_MzJ[1]
									}else{
										nivel=c("0","1212","1434","1767","110910")
										matrizMZ[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
									}
								}else{
									nivel=c("0","1212","1434","1767","110910")
									matrizMZ[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
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
		for(h in c(1,3)){
			if(h==1) {nivel=c("0");numero=0}
			if(h!=1) {nivel=c("1212","1434","1767","110910");numero=1}
			for(k in 1:length(nivel)){
				numero=numero+1	
#			print(k)
			#print(numero)

				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in c(1,3)){
							if(l==1) {nivel=c("0");numeros=0}
							if(l!=1) {nivel=c("1212","1434","1767","110910");numeros=1}
							for(o in 1:length(nivel)){
								numeros=numeros+1
#print(o)
#							print(numeros)
								b<-(a[[h]][[i]][[j]][[k]]$FreG_Fre[1]-a[[l]][[m]][[n]][[o]]$FreG_Fre[1])/sqrt(a[[h]][[i]][[j]][[k]]$FreG_Fre[2]/100+a[[l]][[m]][[n]][[o]]$FreG_Fre[2]/100)
#		print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$FreG_Fre[2]!=0 & a[[l]][[m]][[n]][[o]]$FreG_Fre[2]!=0){
									if(b>zeta | b<(-zeta)){
										nivel=c("0","1212","1434","1767","110910")
										matrizFR[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-a[[h]][[i]][[j]][[k]]$FreG_Fre[1]-a[[l]][[m]][[n]][[o]]$FreG_Fre[1]
									}else{
										nivel=c("0","1212","1434","1767","110910")
										matrizFR[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
									}
								}else{
									nivel=c("0","1212","1434","1767","110910")
									matrizFR[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
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
		for(h in c(1,3)){
			if(h==1) {nivel=c("0");numero=0}
			if(h!=1) {nivel=c("1212","1434","1767","110910");numero=1}
			for(k in 1:length(nivel)){
				numero=numero+1	
#			print(k)
			#print(numero)

				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in c(1,3)){
							if(l==1) {nivel=c("0");numeros=0}
							if(l!=1) {nivel=c("1212","1434","1767","110910");numeros=1}
							for(o in 1:length(nivel)){
								numeros=numeros+1
#print(o)
#							print(numeros)
								b<-(a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[o]]$CbG_CbJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$CbG_CbJ[2]/100+a[[l]][[m]][[n]][[o]]$CbG_CbJ[2]/100)
#		print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$CbG_CbJ[2]!=0 & a[[l]][[m]][[n]][[o]]$CbG_CbJ[2]!=0){
									if(b>zeta | b<(-zeta)){
										nivel=c("0","1212","1434","1767","110910")
										matrizCB[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-a[[h]][[i]][[j]][[k]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[o]]$CbG_CbJ[1]
									}else{
										nivel=c("0","1212","1434","1767","110910")
										matrizCB[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
									}
								}else{
									nivel=c("0","1212","1434","1767","110910")
									matrizCB[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
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
		for(h in c(1,3)){
			if(h==1) {nivel=c("0");numero=0}
			if(h!=1) {nivel=c("1212","1434","1767","110910");numero=1}
			for(k in 1:length(nivel)){
				numero=numero+1	
#			print(k)
			#print(numero)

				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in c(1,3)){
							if(l==1) {nivel=c("0");numeros=0}
							if(l!=1) {nivel=c("1212","1434","1767","110910");numeros=1}
							for(o in 1:length(nivel)){
								numeros=numeros+1
#print(o)
#							print(numeros)
								b<-(a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[l]][[m]][[n]][[o]]$Quelites[1])/sqrt(a[[h]][[i]][[j]][[k]]$Quelites[2]/100+a[[l]][[m]][[n]][[o]]$Quelites[2]/100)
#		print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$Quelites[2]!=0 & a[[l]][[m]][[n]][[o]]$Quelites[2]!=0){
									if(b>zeta | b<(-zeta)){
										nivel=c("0","1212","1434","1767","110910")
										matrizQ[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[l]][[m]][[n]][[o]]$Quelites[1]
									}else{
										nivel=c("0","1212","1434","1767","110910")
										matrizQ[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
									}
								}else{
									nivel=c("0","1212","1434","1767","110910")
									matrizQ[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
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
		for(h in c(1,3)){
			if(h==1) {nivel=c("0");numero=0}
			if(h!=1) {nivel=c("1212","1434","1767","110910");numero=1}
			for(k in 1:length(nivel)){
				numero=numero+1	
#			print(k)
			#print(numero)

				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in c(1,3)){
							if(l==1) {nivel=c("0");numeros=0}
							if(l!=1) {nivel=c("1212","1434","1767","110910");numeros=1}
							for(o in 1:length(nivel)){
								numeros=numeros+1
#print(o)
#							print(numeros)
								b<-(a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[l]][[m]][[n]][[o]]$Quelites[1])/sqrt(a[[h]][[i]][[j]][[k]]$Quelites[2]/100+a[[l]][[m]][[n]][[o]]$Quelites[2]/100)
#		print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[k]]$Quelites[2]!=0 & a[[l]][[m]][[n]][[o]]$Quelites[2]!=0){
									if(b>zeta | b<(-zeta)){
										nivel=c("0","1212","1434","1767","110910")
										matrizSH[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-a[[h]][[i]][[j]][[k]]$Quelites[1]-a[[l]][[m]][[n]][[o]]$Quelites[1]
									}else{
										nivel=c("0","1212","1434","1767","110910")
										matrizSH[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
									}
								}else{
									nivel=c("0","1212","1434","1767","110910")
									matrizSH[length(manejo)*(j-1)*length(nivel)+(i-1)*length(nivel)+numero, length(manejo)*(n-1)*length(nivel)+(m-1)*length(nivel)+numeros]<-0
								}
							}
						}
					}
				}
			}
		}
	}
}
pdf("~/Dropbox/Chido/comparaciones/diversidades_manejos_perturbaciones_niveles/todos_vs_todos_arvenses.pdf",height=10, width=16)
myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz",manejo="otro")
myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol",manejo="otro")
myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza",manejo="otro")
myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites",manejo="otro")
myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta",manejo="otro")
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
pdf("~/Dropbox/Chido/comparaciones/diversidades_manejos_perturbaciones_niveles/sentido_vs_sentido_arvenses.pdf",height=10, width=16)
source("~/Dropbox/Chido/comparaciones/diversidades_manejos_perturbaciones_niveles/plotImage_filtrado_mz.R")
myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz \n Perturbando a las arvenses, todos los niveles, control determinista",manejo="otro")
source("~/Dropbox/Chido/comparaciones/diversidades_manejos_perturbaciones_niveles/plotImage_filtrado_fre.R")
myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol\n Perturbando a las arvenses, todos los niveles, control determinista",manejo="otro")
source("~/Dropbox/Chido/comparaciones/diversidades_manejos_perturbaciones_niveles/plotImage_filtrado_cb.R")
myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza\n Perturbando a las arvenses, todos los niveles, control determinista",manejo="otro")
source("~/Dropbox/Chido/comparaciones/diversidades_manejos_perturbaciones_niveles/plotImage_todos_vs_todos.R")
myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites\n Perturbando a las arvenses, todos los niveles, control determinista",manejo="otro")
myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta\n Perturbando a las arvenses, todos los niveles, control determinista",manejo="otro")
dev.off()
