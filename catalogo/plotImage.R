# ----- Define a function for plotting a matrix ----- #
myImagePlot <- function(x,title, clave,...){
     min <- min(x)
     max <- max(x)
     yLabels <-c("Temperatura", "Presión", "Precipitación", "Herbívoros", "Depredadores", "MaízG", "FrijolEG", "CalabazaG", "Maíz", "FrijolE", "Calabaza", "MaízJ", "CalabazaJ", "Polinizadores", "FloresNoQuelites", "NoQuelites", "FloresQuelites", "Quelites", "FloresBorde", "Borde", "Desyerbe", "Herbicida", "Plaguicida")
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

layout(mat=c(1,1), widths=c(4,1), heights=c(1,1))
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
 par(mar = c(4,7,4,2),xpd=T)
 image(1:length(xLabels), 1:length(yLabels), t(x), col=ColorRamp, xlab="Bimestres\n",
 ylab="Nodos \n\n", axes=FALSE, zlim=c(min,max))
legend(0.5,26,legend=c("Activo","Inactivo"),horiz=T,border="black",fill=c("yellow","blue"))
 if( !is.null(title) ){
    title(main=paste0("Clave: ",clave,"\n Tamaño de la cuenca de atracción: ",title,"\n"))
 }
axis(BELOW<-1, at=1:length(xLabels), labels=xLabels, cex.axis=0.7)
 axis(LEFT <-2, at=1:length(yLabels), labels=yLabels, las= HORIZONTAL<-1,
 cex.axis=0.7)


# # Color Scale
# par(mar = c(3,2.5,2.5,2))
# image(1, ColorLevels,
#      matrix(data=ColorLevels, ncol=length(ColorLevels),nrow=1),
#      col=ColorRamp,
#      xlab="",ylab="",
#      xaxt="n",oldstyle=T)

# layout(1)
}
# ----- END plot function ----- #
