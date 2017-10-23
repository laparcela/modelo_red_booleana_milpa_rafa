source("~/Dropbox/Chido/catalogo/plotAtr.R")

manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb")

for(i in 1:length(diversidad)){
	for(j in 1:length(manejo)){
		load(paste0("~/Dropbox/Chido/catalogo/",diversidad[i],"_",manejo[j],"_det_0.RData"))

		plotAtr(lista.repe,titulo=paste0("~/Dropbox/Chido/catalogo/",diversidad[i],"_",manejo[j],"_det_0.pdf"),diversidad=diversidad[i],manejo=manejo[j],perturbacion="det",nivel="0")

	}
}
