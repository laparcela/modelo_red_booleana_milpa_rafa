source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_todos_vs_todos(copia).R")
load("~/Dropbox/Chido/permanencias.RData")
a<-permanencias.perturbacion
perturbaciones.main<-c("Sin perturbar","Perturbando las lluvias (sequías)", "Perturbando a las arveneses (malezas)", "Perturbando a los herbívoros (plagas)")
perturbacion=c("det","precipitacion","arvenses","herbivoros")
#manejo=c("desyer")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
#manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb")
nivel=c("1212", "1434", "1767", "110910")
#nivel=c("0","1212", "1434", "1767", "110910")
dimen=(length(manejo)*length(diversidad))
matrizMZ<-matrix(0,dimen,dimen)
matrizFR<-matrix(0,dimen,dimen)
matrizCB<-matrix(0,dimen,dimen)
matrizQ<-matrix(0,dimen,dimen)
matrizSH<-matrix(0,dimen,dimen)
alfa=1-(1-0.05)^(1/(dimen-1))
zeta=qnorm(1-(alfa/2))
#numero=0
pdf(paste0("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/diversidad_vs_diversidad_119010.pdf"),height=7,width=10)
for(h in 1:length(perturbacion)){
	for(i in 1:length(manejo)){
		for(j in 1:length(diversidad)){
			if(h==1) {nivel=c("0"); level=1}
			if(h!=1) {nivel=c("110910"); level=4}
#			for(k in level){	
#				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(n in 1:length(diversidad)){
#							if(l==1) {nivel=c("0"); level=1}
#							if(l!=1) {nivel=c("110910"); level=4}
#							for(o in level){
#								numero=numero+1
#								print(numero)
								b<-(a[[h]][[i]][[j]][[level]]$MzG_MzJ[1]-a[[h]][[m]][[n]][[level]]$MzG_MzJ[1])/sqrt(a[[h]][[i]][[j]][[level]]$MzG_MzJ[2]/100+a[[l]][[m]][[n]][[level]]$MzG_MzJ[2]/100)
#								print(b)
								if(b!="NaN" & a[[h]][[i]][[j]][[level]]$MzG_MzJ[2]!=0 & a[[l]][[m]][[n]][[level]]$MzG_MzJ[2]!=0){
									if(b>zeta | b<(-zeta)){
										matrizMZ[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-a[[h]][[i]][[j]][[level]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[level]]$MzG_MzJ[1]
									}else{
										matrizMZ[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
									}
								}else{
										matrizMZ[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
								}
#							}
						}
					}
#				}
#			}
		}
	}
myImagePlot(matrizMZ,paste0("Comparaciones múltiples de la permanencia del maíz\n",perturbaciones.main[h]," Nivel de pert: 0.1"),manejo="otro")
}
#for(h in 1:length(perturbacion)){
#	for(i in 1:length(manejo)){
#		for(j in 1:length(diversidad)){
#			if(h==1) {nivel=c("0"); level=1}
#			if(h!=1) {nivel=c("110910"); level=4}
##			for(k in level){	
##				for(n in 1:length(diversidad)){
#					for(m in 1:length(manejo)){
#						for(n in 1:length(diversidad)){
##							if(l==1) {nivel=c("0"); level=1}
##							if(l!=1) {nivel=c("110910"); level=4}
##							for(o in level){
##								numero=numero+1
##								print(numero)
#								b<-(a[[h]][[i]][[j]][[level]]$CbG_CbJ[1]-a[[h]][[m]][[n]][[level]]$CbG_CbJ[1])/sqrt(a[[h]][[i]][[j]][[level]]$CbG_CbJ[2]/100+a[[l]][[m]][[n]][[level]]$CbG_CbJ[2]/100)
##								print(b)
#								if(b!="NaN" & a[[h]][[i]][[j]][[level]]$CbG_CbJ[2]!=0 & a[[l]][[m]][[n]][[level]]$CbG_CbJ[2]!=0){
#									if(b>zeta | b<(-zeta)){
#										matrizCB[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-a[[h]][[i]][[j]][[level]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[level]]$CbG_CbJ[1]
#									}else{
#										matrizCB[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
#									}
#								}else{
#										matrizCB[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
#								}
##							}
#						}
#					}
##				}
##			}
#		}
#	}
#myImagePlot(matrizFR,paste0("Comparaciones múltiples de la permanencia del frijol\n",perturbaciones.main[h]," Nivel de pert: 0.1"),manejo="otro")
#}
#for(h in 1:length(perturbacion)){
#	for(i in 1:length(manejo)){
#		for(j in 1:length(diversidad)){
#			if(h==1) {nivel=c("0"); level=1}
#			if(h!=1) {nivel=c("110910"); level=4}
##			for(k in level){	
##				for(n in 1:length(diversidad)){
#					for(m in 1:length(manejo)){
#						for(n in 1:length(diversidad)){
##							if(l==1) {nivel=c("0"); level=1}
##							if(l!=1) {nivel=c("110910"); level=4}
##							for(o in level){
##								numero=numero+1
##								print(numero)
#								b<-(a[[h]][[i]][[j]][[level]]$CbG_CbJ[1]-a[[h]][[m]][[n]][[level]]$CbG_CbJ[1])/sqrt(a[[h]][[i]][[j]][[level]]$CbG_CbJ[2]/100+a[[l]][[m]][[n]][[level]]$CbG_CbJ[2]/100)
##								print(b)
#								if(b!="NaN" & a[[h]][[i]][[j]][[level]]$CbG_CbJ[2]!=0 & a[[l]][[m]][[n]][[level]]$CbG_CbJ[2]!=0){
#									if(b>zeta | b<(-zeta)){
#										matrizCB[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-a[[h]][[i]][[j]][[level]]$CbG_CbJ[1]-a[[l]][[m]][[n]][[level]]$CbG_CbJ[1]
#									}else{
#										matrizCB[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
#									}
#								}else{
#										matrizCB[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
#								}
##							}
#						}
#					}
##				}
##			}
#		}
#	}
#myImagePlot(matrizCB,paste0("Comparaciones múltiples de la permanencia de la calabaza\n",perturbaciones.main[h]," Nivel de pert: 0.1"),manejo="otro")
#}
#for(h in 1:length(perturbacion)){
#	for(i in 1:length(manejo)){
#		for(j in 1:length(diversidad)){
#			if(h==1) {nivel=c("0"); level=1}
#			if(h!=1) {nivel=c("110910"); level=4}
##			for(k in level){	
##				for(n in 1:length(diversidad)){
#					for(m in 1:length(manejo)){
#						for(n in 1:length(diversidad)){
##							if(l==1) {nivel=c("0"); level=1}
##							if(l!=1) {nivel=c("110910"); level=4}
##							for(o in level){
##								numero=numero+1
##								print(numero)
#								b<-(a[[h]][[i]][[j]][[level]]$Quelites[1]-a[[h]][[m]][[n]][[level]]$Quelites[1])/sqrt(a[[h]][[i]][[j]][[level]]$Quelites[2]/100+a[[l]][[m]][[n]][[level]]$Quelites[2]/100)
##								print(b)
#								if(b!="NaN" & a[[h]][[i]][[j]][[level]]$Quelites[2]!=0 & a[[l]][[m]][[n]][[level]]$Quelites[2]!=0){
#									if(b>zeta | b<(-zeta)){
#										matrizQ[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-a[[h]][[i]][[j]][[level]]$Quelites[1]-a[[l]][[m]][[n]][[level]]$Quelites[1]
#									}else{
#										matrizQ[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
#									}
#								}else{
#										matrizQ[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
#								}
##							}
#						}
#					}
##				}
##			}
#		}
#	}
#myImagePlot(matrizQ,paste0("Comparaciones múltiples de la permanencia de los quelites\n",perturbaciones.main[h]," Nivel de pert: 0.1"),manejo="otro")
#}
#for(h in 1:length(perturbacion)){
#	for(i in 1:length(manejo)){
#		for(j in 1:length(diversidad)){
#			if(h==1) {nivel=c("0"); level=1}
#			if(h!=1) {nivel=c("110910"); level=4}
##			for(k in level){	
##				for(n in 1:length(diversidad)){
#					for(m in 1:length(manejo)){
#						for(n in 1:length(diversidad)){
##							if(l==1) {nivel=c("0"); level=1}
##							if(l!=1) {nivel=c("110910"); level=4}
##							for(o in level){
##								numero=numero+1
##								print(numero)
#								b<-(a[[h]][[i]][[j]][[level]]$conj[1]-a[[h]][[m]][[n]][[level]]$conj[1])/sqrt(a[[h]][[i]][[j]][[level]]$conj[2]/100+a[[l]][[m]][[n]][[level]]$conj[2]/100)
##								print(b)
#								if(b!="NaN" & a[[h]][[i]][[j]][[level]]$conj[2]!=0 & a[[l]][[m]][[n]][[level]]$conj[2]!=0){
#									if(b>zeta | b<(-zeta)){
#										matrizSH[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-a[[h]][[i]][[j]][[level]]$conj[1]-a[[l]][[m]][[n]][[level]]$conj[1]
#									}else{
#										matrizSH[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
#									}
#								}else{
#										matrizSH[length(diversidad)*(i-1)+j, length(diversidad)*(m-1)+n]<-0
#								}
##							}
#						}
#					}
##				}
##			}
#		}
#	}
#myImagePlot(matrizSH,paste0("Comparaciones múltiples de la permanencia conjunta\n",perturbaciones.main[h]," Nivel de pert: 0.1"),manejo="otro")
#}
dev.off()
#cerosMZ<-which(apply(matrizMZ,1,sum)==0)
#matrizMZ<-abs(matrizMZ[-cerosMZ,-cerosMZ])
#cerosFR<-which(apply(matrizFR,1,sum)==0)
#matrizFR<-abs(matrizFR[-cerosFR,-cerosFR])
#cerosCB<-which(apply(matrizCB,1,sum)==0)
#matrizCB<-abs(matrizCB[-cerosCB,-cerosCB])
##cerosQ<-which(apply(matrizQ,1,sum)==0)
#matrizQ<-abs(matrizQ)#[-cerosQ,-cerosQ])
##cerosSH<-which(apply(matrizSH,1,sum)==0)
#matrizSH<-abs(matrizSH)#[-cerosSH,-cerosSH]
#pdf("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/sentido_vs_sentido_110910_vs_0.pdf",height=10, width=16)
#source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_filtrado_mz.R")
#myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz \n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
#source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_filtrado_fre.R")
#myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol\n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
#source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_filtrado_cb.R")
#myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza\n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
#source("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/plotImage_todos_vs_todos.R")
#myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites\n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
#myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta\n Todas las perturbaciones, nivel=1/2, control determinista",manejo="otro")
#dev.off()
