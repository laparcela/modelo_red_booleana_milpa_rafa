#source("~/Dropbox/Chido/plotImage.R")
load("~/Dropbox/Chido/permanencias.RData")
a<-permanencias.perturbacion

perturbacion=c("det","precipitacion")
#,"arvenses","herbivoros")
#manejo=c("desyer")
manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
#manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup")
diversidad=c("milpa", "mzcb", "mzfre", "mz")
#, "cb")
#nivel=c("1212", "1434", "1767", "110910")
dimen=(length(perturbacion)*length(manejo)*length(diversidad))
matrizMZ<-matrix(0,dimen,dimen)
matrizFR<-matrix(0,dimen,dimen)
matrizCB<-matrix(0,dimen,dimen)
matrizQ<-matrix(0,dimen,dimen)
matrizSH<-matrix(0,dimen,dimen)
alfa=1-(1-0.05)^(1/(dimen-1))
zeta=qnorm(1-(alfa/2))
#numero=0
for(j in 1:length(diversidad)){
	for(i in 1:length(manejo)){
		for(h in 1:length(perturbacion)){
			if(h==1) nivel=c("0")
			if(h!=1) nivel=c("110910")
			for(k in 1:length(nivel)){
				for(n in 1:length(diversidad)){
					for(m in 1:length(manejo)){
						for(l in 1:length(perturbacion)){
							if(l==1) nivel=c("0")
							if(l!=1) nivel=c("110910")
							for(o in 1:length(nivel)){
#								numero=numero+1
#								print(numero)
								b<-(a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[o]]$MzG_MzJ[1])/sqrt(a[[h]][[i]][[j]][[k]]$MzG_MzJ[2]+a[[l]][[m]][[n]][[o]]$MzG_MzJ[2])
#								print(b)
								if(b!="NaN"){
									if(b>zeta | b<(-zeta)){
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(i-1)*length(nivel)+length(nivel)*(h-1)+k, length(manejo)*(n-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(m-1)*length(nivel)+length(nivel)*(l-1)+o]<-abs(a[[h]][[i]][[j]][[k]]$MzG_MzJ[1]-a[[l]][[m]][[n]][[o]]$MzG_MzJ[1])
									}else{
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(i-1)*length(nivel)+length(nivel)*(h-1)+k, length(manejo)*(n-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(m-1)*length(nivel)+length(nivel)*(l-1)+o]<-0
									}
								}else{
										matrizMZ[length(manejo)*(j-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(i-1)*length(nivel)+length(nivel)*(h-1)+k, length(manejo)*(n-1)*length(perturbacion)*length(nivel)+length(perturbacion)*(m-1)*length(nivel)+length(nivel)*(l-1)+o]<-0
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
# ----- Define a function for plotting a matrix ----- #
myImagePlot <- function(x,title,manejo=c("otro","det"),...){
     min <- min(x)
     max <- max(x)
     yLabels <- rownames(x)
     xLabels <- colnames(x)
#     title <-c()
  # check for additional function arguments
  if( length(list(...)) ){
    Lst <- list(...)
    if( !is.null(Lst$zlim) ){
       min <- Lst$zlim[1]
       max <- Lst$zlim[2]
    }
    if( !is.null(Lst$yLabels) ){
       yLabels <- c(Lst$yLabels)
    }
    if( !is.null(Lst$xLabels) ){
       xLabels <- c(Lst$xLabels)
    }
    if( !is.null(Lst$title) ){
       title <- Lst$title
    }
  }
# check for null values
if( is.null(xLabels) ){
   xLabels <- c(1:ncol(x))
}
if( is.null(yLabels) ){
   yLabels <- c(1:nrow(x))
}

layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,0.5), heights=c(1,1))

 # Red and green range from 0 to 1 while Blue ranges from 1 to 0
 ColorRamp <- rgb( seq(0,1,length=256),  # Red
                   seq(0,1,length=256),  # Green
                   seq(1,0,length=256))  # Blue
 ColorLevels <- seq(min, max, length=length(ColorRamp))

 # Reverse Y axis
 reverse <- nrow(x) : 1
 yLabels <- yLabels[reverse]
 x <- x[reverse,]

 # Data Map
 par(mar = c(5,10,7,2))
 image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="",
 ylab="", axes=FALSE, zlim=c(min,max))
 if( !is.null(title) ){
    title(sub=title,cex.sub=0.9,line=2)
 }

if(manejo=="otro"){
#axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
 axis(LEFT <-3, at=c(0.5,10.5,20.5,30.5,40.5), labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7,line=3)
 axis(LEFT <-3, at=c(5,15,25,35), labels=c("milpa", "mzcb", "mzfre", "mz"), las= HORIZONTAL<-1, cex.axis=0.9, line=3,tick=F)

 axis(LEFT <-3, at=seq(0.5,40.5,2), labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7)
 axis(LEFT <-3, at=seq(1.5, 39.5,2), labels=rep(c("Desyerbe", "Desyerbe \nPlaguicida", "Herbicida",  "Plaguicida \nHerbicida", "Roundup" ),4), las= HORIZONTAL<-1, cex.axis=0.75,tick=F)

 axis(LEFT <-2, at=c(0.5,10.5,20.5,30.5,40.5), labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7,line=5.5)
 axis(LEFT <-2, at=c(5,15,25,35), labels=c("mz", "mzfre", "mzcb","milpa" ), las= HORIZONTAL<-1, cex.axis=0.9, line=5.5,tick=F)

 axis(LEFT <-2, at=seq(0.5,40.5,2), labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7)
 axis(LEFT <-2, at=seq(1.5, 39.5,2), labels=rep(c("Roundup", "Plaguicida \nHerbicida", "Herbicida", "Desyerbe \nPlaguicida", "Desyerbe" ),4), las= HORIZONTAL<-1, cex.axis=0.85,tick=F)

#axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
}

if(manejo=="det"){
 axis(LEFT <-3, at=c(0.5,5.5,10.5,15.5,20.5,25.5), labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7,line=3)
 axis(LEFT <-3, at=c(2.5,7.5,12.5,17.5,22.5), labels=c("Desyerbe", "Desyerbe \nPlaguicida", "Herbicida",  "Plaguicida \nHerbicida", "Roundup" ), las= HORIZONTAL<-1, cex.axis=0.9, line=3,tick=F)

 axis(LEFT <-3, at=seq(0.5,25.5,1), labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7)
 axis(LEFT <-3, at=seq(1,25,1), labels=rep(c("milpa", "mzcb", "mzfre", "mz","cb" ),5), las= HORIZONTAL<-1, cex.axis=0.85,tick=F)

 axis(LEFT <-2, at=c(0.5,5.5,10.5,15.5,20.5,25.5), labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7,line=4)
 axis(LEFT <-2, at=c(2.5,7.5,12.5,17.5,22.5), labels=c("Roundup", "Plaguicida \nHerbicida", "Herbicida", "Desyerbe \nPlaguicida", "Desyerbe" ), las= HORIZONTAL<-1, cex.axis=0.9, line=4,tick=F)

 axis(LEFT <-2, at=seq(0.5,25.5,1), labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7)
 axis(LEFT <-2, at=seq(1,25,1), labels=rep(c("cb", "mz", "mzfre", "mzcb","milpa" ),5), las= HORIZONTAL<-1, cex.axis=0.9,tick=F)
}
 # Color Scale
 par(mar = c(3,2.5,2.5,2))
 image(1, ColorLevels,
      matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
      col=ColorRamp,
      xlab="",ylab="",
      xaxt="n",oldstyle=T)

 layout(1)
}
# ----- END plot function ----- #


pdf("resultados1.pdf",height=10, width=16)
myImagePlot(matrizMZ,"Comparaciones múltiples de la permanencia promedio del maíz",manejo="otro")
#myImagePlot(matrizFR,"Comparaciones múltiples de la permanencia promedio del frijol",manejo="otro")
#myImagePlot(matrizCB,"Comparaciones múltiples de la permanencia promedio de la calabaza",manejo="otro")
#myImagePlot(matrizQ,"Comparaciones múltiples de la permanencia promedio de los quelites",manejo="otro")
#myImagePlot(matrizSH,"Comparaciones múltiples de la permanencia conjunta (Shannon)",manejo="otro")
dev.off()
