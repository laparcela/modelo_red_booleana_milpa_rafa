##Llamamos a la función salvadora!!!!, gracias!!!:http://www.phaget4.org/R/image_matrix.html
source("~/Dropbox/Chido/plotImage.R")
plotAtr <-function(atr,titulo){
##Este ciclo imprime los atractores de manera similar a como lo hace la función "plotAttractors" de BoolNet.

#jpeg(titulo,height=800,width=480,quality=100)
pdf(titulo,height=7,width=12)	
# 	par(mfrow=c(3,1))
	for(j in 1:length(atr)){
		if(is.null(atr[[j]])){
			next
		}else{
			if(is.null(dim(atr[[j]]))){
				next
			}else{
				if(ncol(atr[[j]])>=6){
#					jpeg(paste0("atrac",j,".jpeg"),width=720)
					myImagePlot(atr[[j]],names(atr)[j])
#			  		dev.off()
				}else{
					next
				}
				
			}
		}
	}
dev.off()
}
