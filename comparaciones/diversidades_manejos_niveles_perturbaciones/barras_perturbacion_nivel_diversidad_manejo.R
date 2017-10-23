load("~/Dropbox/Chido/permanencias.RData")

a<-permanencias.perturbacion

perturbacion=c("det","precipitacion","arvenses","herbivoros")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa","mzcb","mzfre","mz","cb")
nivel=c("1212","1434","1767","110910")

perturbaciones<-c("Precipitación","Arvenses","Herbívoros")
diversidades<-c("milpa (maíz, frijol enredador y calabaza)", "maíz y calabaza", "maíz y frijol enredador", "maíz", "calabaza")
probabilidades<-c("1/2","1/4","1/7","1/10")
manejos<-c("desyerbe manual","desyerbe manual y plaguicida", "herbicida", "plaguicida y herbicida", "plaguicida y herbicida (Roundup)")
perturbaciones.main<-c("las lluvias (sequías)", "las arveneses (malezas)", "los herbívoros (plagas)")

lista.manejos<-list()
lista.diversidad<-list()
lista.perturbacion<-list()
lista.nivel<-list()

for(k in 1:length(nivel)){
for(h in c(2,3,4)){
	for(j in c(2,5,3,4,1)){
		for(i in 1:length(manejo)){

				barras_trat<-matrix(unlist(a[[h]][[i]][[j]][[k]]),5,2,byrow=T,dimnames=list(c("Permanencia maíz","Permanencia frijol", "Permanencia calabaza", "Permanencia quelites", "Permanencia conjunta"),c(paste0("Permanencia ",probabilidades[k],"\n ",manejo[i]), paste0("Error estándar (",probabilidades[k],")"))))
				barras_det<-matrix(unlist(a[[1]][[i]][[j]][[1]]),5,2,byrow=T,dimnames=list(c("Permanencia maíz","Permanencia frijol", "Permanencia calabaza", "Permanencia quelites", "Permanencia conjunta"),c(paste0("Permanencia (det)\n ",manejo[i]), "Error estándar (det)")))
			lista.manejos[[i]]<-cbind(barras_trat,barras_det)

		}
		lista.diversidad[[j]]<-do.call(cbind,lista.manejos)
	}
	names(lista.diversidad)<-diversidad
#Genera pdf de las proporciones...
	pdf(paste0("~/Dropbox/Chido/comparaciones/diversidades_manejos_niveles_perturbaciones/barras_perturbacion_nivel_diversidad_manejo/barras_",perturbacion[h],"_",nivel[k],"_vs_0.pdf"),height=10,width=18)
	for(i in 1:length(lista.diversidad)){
		b<-barplot(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)], ylim=c(0,max(lista.diversidad[[i]])+0.1), beside=T, col=c("light green", "dark green", "yellow", "orange", "red"), border="purple", ylab="\nPermanencias",main=paste0("\n Permanencias individuales de cultivos, quelites y permanencia conjunta \n bajo distintos tipos de manejo, perturbando la incidencia de precipitaciones (P=",probabilidades[k],") n=100)\n Comparando varios tipos de manejo: tradicional(desyerbe manual) y convencional(plaguicida y herbicida)\n Cultivos: ",diversidades[i]), legend.text=T,args.legend = list(x=60.5, y=max(lista.diversidad[[i]]+0.1), bty = "n"))
		arrows(b, as.vector(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)]+sqrt(lista.diversidad[[i]][,seq(2,ncol(lista.diversidad[[i]]),2)])), b, as.vector(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)]), angle=90, code=1, length=0.05)
		arrows(b, as.vector(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)]-sqrt(lista.diversidad[[i]][,seq(2,ncol(lista.diversidad[[i]]),2)])), b, as.vector(lista.diversidad[[i]][,seq(1,ncol(lista.diversidad[[i]]),2)]), angle=90, code=1, length=0.05)
	}
	dev.off()
	lista.perturbacion[[h-1]]<-do.call(cbind,lista.diversidad)
}
names(lista.perturbacion)<-perturbacion[-1]
lista.perturbacion
}

