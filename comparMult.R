source("~/Dropbox/Chido/cuantiAtr.R")

perturbacion=c("det","precipitacion", "arvenses", "herbivoros")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb")

permanencias.nivel<-list()
permanencias.diversidad<-list()
permanencias.manejo<-list()
permanencias.perturbacion<-list()

for(i in 1:length(perturbacion)){
if(i==1) nivel=c("0")
if(i!=1) nivel=c("1212", "1434", "1767", "110910")
	for(j in 1:length(manejo)){
		for(k in 1:length(diversidad)){
			for(l in 1:length(nivel)){
					load(paste0("~/Dropbox/Chido/",perturbacion[i],"/",manejo[j],"/",diversidad[k],"/",diversidad[k],"_",manejo[j],"_",perturbacion[i],"_",nivel[l],".RData"))
					res<-lista.repe
					permanencias.nivel[[l]]<-cuantiAtr(res)
					names(permanencias.nivel[[l]])<-c("MzG_MzJ","FreG_Fre","CbG_CbJ","Quelites","conj")
			}
			names(permanencias.nivel)<-nivel
			permanencias.diversidad[[k]]<-permanencias.nivel
		}
		names(permanencias.diversidad)<-diversidad
		permanencias.manejo[[j]]<-permanencias.diversidad
	}
	names(permanencias.manejo)<-manejo
	permanencias.perturbacion[[i]]<-permanencias.manejo
}
names(permanencias.perturbacion)<-perturbacion

save(permanencias.perturbacion,file=paste0("~/Dropbox/Chido/permanencias.RData"))
print(permanencias.perturbacion)
