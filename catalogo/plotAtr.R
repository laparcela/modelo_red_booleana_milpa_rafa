##Llamamos a la función salvadora!!!!, gracias!!!:http://www.phaget4.org/R/image_matrix.html
source("~/Dropbox/Chido/catalogo/plotImage.R")
plotAtr <-function(atr,titulo,diversidad,manejo,perturbacion,nivel){
##Este ciclo imprime los atractores de manera similar a como lo hace la función "plotAttractors" de BoolNet.
# 	par(ask=TRUE)
pdf(titulo,height=7,width=12)	
	for(j in 1:length(atr[[1]])){
		if(is.null(atr[[1]][[j]])){
			next
		}else{
			if(is.null(dim(atr[[1]][[j]]))){
				next
			}else{
				if(ncol(atr[[1]][[j]])>=6){
#					jpeg(paste0("atrac",j,".jpeg"),width=720)
					myImagePlot(atr[[1]][[j]],title=names(atr[[1]])[[j]],clave=paste0(diversidad,"_",manejo,"_",perturbacion,"_",nivel))
#			  		dev.off()
				}else{
					next
				}
				
			}
		}
	}
dev.off()
}
