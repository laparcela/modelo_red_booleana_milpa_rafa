load("~/Dropbox/Chido/Experimento1/milpa/milpa_trad_per_herb_1212_100_3000.RData")
a1<-cuantiAtr(lista.repe)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_conv_per_herb_1212_100_3000.RData")
b1<-cuantiAtr(lista.repe)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_trad_per_herb_1434_100_3000.RData")
a2<-cuantiAtr(lista.repe)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_conv_per_herb_1434_100_3000.RData")
b2<-cuantiAtr(lista.repe)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_trad_per_herb_1767_100_3000.RData")
a3<-cuantiAtr(lista.repe)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_conv_per_herb_1767_100_3000.RData")
b3<-cuantiAtr(lista.repe)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_trad_per_herb_110910_100_3000.RData")
a4<-cuantiAtr(lista.repe)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_conv_per_herb_110910_100_3000.RData")
b4<-cuantiAtr(lista.repe)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_trad_30000.RData")
a0<-cuantiAtr(prue)
load("~/Dropbox/Chido/Experimento1/milpa/milpa_conv_30000.RData")
b0<-cuantiAtr(prue)

#Genera .csv de las proporciones
res<-cbind(as.matrix(unlist(a1[1:8])),as.matrix(unlist(b1[1:8])),as.matrix(unlist(a2[1:8])),as.matrix(unlist(b2[1:8])),as.matrix(unlist(a3[1:8])),as.matrix(unlist(b3[1:8])),as.matrix(unlist(a4[1:8])),as.matrix(unlist(b4[1:8])),as.matrix(unlist(a0[1:8])),as.matrix(unlist(b0[1:8])))
rownames(res)<-Cultivos
colnames(res)<-ManejosPer
write.csv(res,"~/Dropbox/Chido/Experimento1/resultados/resultados_milpa_per_herb.csv")

prop<-matrix(0,5,10)
	for(i in 1:ncol(res)){
		prop[1,i]<-res[3,i]/res[1,i]
		prop[2,i]<-res[3,i]/res[2,i]
		prop[3,i]<-res[5,i]/res[4,i]
		prop[4,i]<-res[8,i]/res[6,i]
		prop[5,i]<-res[8,i]/res[7,i]
	}
	rownames(prop)<-Ratios
	colnames(prop)<-ManejosPer
prop<-ifelse(is.finite(prop),prop,0)

res0<-cbind(as.matrix(unlist(a1)[c(9,11,13,15,17)]),as.matrix(unlist(b1)[c(9,11,13,15,17)]),as.matrix(unlist(a2)[c(9,11,13,15,17)]),as.matrix(unlist(b2)[c(9,11,13,15,17)]),as.matrix(unlist(a3)[c(9,11,13,15,17)]),as.matrix(unlist(b3)[c(9,11,13,15,17)]),as.matrix(unlist(a4)[c(9,11,13,15,17)]),as.matrix(unlist(b4)[c(9,11,13,15,17)]),as.matrix(unlist(a0)[c(9,11,13,15,17)]),as.matrix(unlist(b0)[c(9,11,13,15,17)]))
rownames(res0)<-Ratios
colnames(res0)<-ManejosPer
write.csv(res0,"~/Dropbox/Chido/Experimento1/resultados/resultados_milpa_per_herb0.csv")

res1<-cbind(as.matrix(unlist(a1)[c(9,11,13,15,17)]), as.matrix(unlist(a1)[c(10,12,14,16,18)]), as.matrix(unlist(b1)[c(9,11,13,15,17)]), as.matrix(unlist(b1)[c(10,12,14,16,18)]), as.matrix(unlist(a2)[c(9,11,13,15,17)]), as.matrix(unlist(a2)[c(10,12,14,16,18)]), as.matrix(unlist(b2)[c(9,11,13,15,17)]), as.matrix(unlist(b2)[c(10,12,14,16,18)]), as.matrix(unlist(a3)[c(9,11,13,15,17)]), as.matrix(unlist(a3)[c(10,12,14,16,18)]), as.matrix(unlist(b3)[c(9,11,13,15,17)]), as.matrix(unlist(b3)[c(10,12,14,16,18)]), as.matrix(unlist(a4)[c(9,11,13,15,17)]), as.matrix(unlist(a4)[c(10,12,14,16,18)]), as.matrix(unlist(b4)[c(9,11,13,15,17)]), as.matrix(unlist(b4)[c(10,12,14,16,18)]), as.matrix(unlist(a0)[c(9,11,13,15,17)]), as.matrix(unlist(a0)[c(10,12,14,16,18)]), as.matrix(unlist(b0)[c(9,11,13,15,17)]), as.matrix(unlist(b0)[c(10,12,14,16,18)]))
rownames(res1)<-Ratios
colnames(res1)<-ManejosPerES
write.csv(res1,"~/Dropbox/Chido/Experimento1/resultados/resultados_milpa_per_herb1.csv")

#Genera pdf de las proporciones...
pdf("~/Dropbox/Chido/Experimento1/resultados/resultados_milpa_per_herb.pdf",height=10,width=18)
	barplot(prop, ylim=c(0,max(prop)+0.1), beside=T, col=c("light green", "dark green", "yellow", "dark orange", "orange"), border="purple", ylab="\nProporción de temporales exitosos",main="Porporción de temporales exitosos perturbando la incidencia de herbívoros (n=100)\n Comparando dos tipos de manejo: tradicional(desyerbe manual) y convencional(plaguicida y herbicida)\n Cultivos: maíz, frijol enredador y calabaza (milpa)",legend.text=T,args.legend = list(x=4.5, y=max(prop), bty = "n"))
	b<-barplot(res0, ylim=c(0,max(res0)+0.1), beside=T, ylab="\nProporción de temporales exitosos", main="\nPorporción PROMEDIO de temporales exitosos perturbando la incidencia de herbívoros (n=100)\n Comparando dos tipos de manejo: tradicional(desyerbe manual) y convencional(plaguicida y herbicida)\n Cultivos: maíz, frijol enredador y calabaza (milpa)\n El promedio se ponderó de acuerdo al tamaño de las cuencas de atracción", col=c("light green", "dark green", "yellow", "dark orange", "orange"), border="purple",legend=T,args.legend = list(x=4.5, y=max(res0), bty = "n"))
	arrows(b, as.vector(res1[,c(1,3,5,7,9,11,13,15,17,19)]+res1[,c(2,4,6,8,10,12,14,16,18,20)]), b, as.vector(res1[,c(1,3,5,7,9,11,13,15,17,19)]), angle=90, code=1, length=0.05)
	arrows(b, as.vector(res1[,c(1,3,5,7,9,11,13,15,17,19)]-res1[,c(2,4,6,8,10,12,14,16,18,20)]), b, as.vector(res1[,c(1,3,5,7,9,11,13,15,17,19)]), angle=90, code=1, length=0.05)
dev.off()
