load("~/Dropbox/Chido/permanencias.RData")

a<-permanencias.perturbacion

perturbacion=c("det","precipitacion","arvenses","herbivoros")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa","mzcb","mzfre","mz","cb")
nivel=c("0","1212","1434","1767","110910")

perturbaciones<-c("Precipitación","Arvenses","Herbívoros")
diversidades<-c("milpa (maíz, frijol enredador y calabaza)", "maíz y calabaza", "maíz y frijol enredador", "maíz", "calabaza")
probabilidades<-c("0","0.5","0.25","0.14","0.1")
manejos<-c("desyerbe manual","desyerbe manual y plaguicida", "herbicida", "plaguicida y herbicida", "plaguicida y herbicida (Roundup)")
perturbaciones.main<-c("las lluvias (sequías)", "las arveneses (malezas)", "los herbívoros (plagas)")

lista.manejos<-list()
lista.diversidad<-list()
lista.perturbacion<-list()
lista.nivel<-list()

#for(k in 1:length(nivel)){
for(h in c(2,3,4)){
	for(j in c(2,5,3,4,1)){
		numero=6
		for(k in 1:length(nivel)){
			numero=numero-1
			if(k==1){
				barras_trad<-matrix(unlist(a[[1]][[1]][[j]][[k]]),5,2,byrow=T,dimnames=list(c("Permanencia maíz","Permanencia frijol", "Permanencia calabaza", "Permanencia quelites", "Permanencia conjunta"),c(paste0("Nivel de pert: ",probabilidades[k],"\n ","Tradicional"), paste0("Error estándar (",probabilidades[k],")"))))
				barras_conv<-matrix(unlist(a[[1]][[4]][[j]][[k]]),5,2,byrow=T,dimnames=list(c("Permanencia maíz","Permanencia frijol", "Permanencia calabaza", "Permanencia quelites", "Permanencia conjunta"),c(paste0("Nivel de pert: ",probabilidades[k],"\n ","Convencional"), "Error estándar (det)")))
			}
			if(k!=1){
				barras_trad<-matrix(unlist(a[[h]][[1]][[j]][[numero]]),5,2,byrow=T,dimnames=list(c("Permanencia maíz","Permanencia frijol", "Permanencia calabaza", "Permanencia quelites", "Permanencia conjunta"),c(paste0("Nivel de pert: ",probabilidades[numero+1],"\n ","Tradicional"), paste0("Error estándar (",probabilidades[k],")"))))
				barras_conv<-matrix(unlist(a[[h]][[4]][[j]][[numero]]),5,2,byrow=T,dimnames=list(c("Permanencia maíz","Permanencia frijol", "Permanencia calabaza", "Permanencia quelites", "Permanencia conjunta"),c(paste0("Nivel de pert: ",probabilidades[numero+1],"\n ","Convencional"), "Error estándar (det)")))
			}
			lista.nivel[[k]]<-cbind(barras_trad,barras_conv)
		}
		names(lista.nivel)<-probabilidades
		lista.diversidad[[j]]<-do.call(cbind,lista.nivel)
	}
#	names(lista.diversidad)<-diversidad

#Genera pdf de las proporciones...
	pdf(paste0("~/Dropbox/Chido/comparaciones/perturbaciones_diversidades_manejos_niveles/barras_",perturbacion[h],"_",diversidad[j],"_trad_vs_conv.pdf"),height=10,width=18)
	for(i in 1:length(lista.diversidad)){
		b<-barplot(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)], ylim=c(0,max(lista.diversidad[[i]])+0.2), beside=T, col=c("light green", "dark green", "yellow", "dark orange", "red"), border="purple", ylab="\nPermanencias",main=paste0("\n Permanencias individuales de cultivos/quelites y permanencia conjunta \n bajo dos tipos de manejo perturbando la incidencia de ",perturbaciones.main[h-1]," a lo largo de un gradiente (n=100)\n Comparando dos tipos de manejo: tradicional(desyerbe manual) y convencional(plaguicida y herbicida)\n Cultivos: ",diversidades[i]), legend.text=T,args.legend = list(x=63, y=max(lista.diversidad[[i]]+0.1), bty = "n"))
		arrows(b, as.vector(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)]+sqrt(lista.diversidad[[i]][,seq(2,ncol(lista.diversidad[[i]]),2)])), b, as.vector(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)]), angle=90, code=1, length=0.05)
		arrows(b, as.vector(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)]-sqrt(lista.diversidad[[i]][,seq(2,ncol(lista.diversidad[[i]]),2)])), b, as.vector(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)]), angle=90, code=1, length=0.05)
	}
	dev.off()
	lista.perturbacion[[h]]<-do.call(cbind,lista.diversidad)
}
#names(lista.manejos)<-manejo
#lista.manejos
#}
