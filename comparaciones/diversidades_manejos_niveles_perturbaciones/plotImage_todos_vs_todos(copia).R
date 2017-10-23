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
 axis(LEFT <-3, at=c(0.5,20.5,40.5,60.5,80.5,100.5)/4, labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7,line=3)
 axis(LEFT <-3, at=c(10,30,50,70,90)/4, labels=c("milpa", "mzcb", "mzfre", "mz","cb" ), las= HORIZONTAL<-1, cex.axis=0.9, line=3,tick=F)

 axis(LEFT <-3, at=seq(0.5,100.5,4)/4, labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7)
 axis(LEFT <-3, at=seq(2.5, 98.5,4)/4, labels=rep(c("Desyerbe", "Desyerbe \nPlaguicida", "Herbicida",  "Plaguicida \nHerbicida", "Roundup" ),5), las= HORIZONTAL<-1, cex.axis=0.75,tick=F)

 axis(LEFT <-2, at=c(0.5,20.5,40.5,60.5,80.5,100.5)/4, labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7,line=5.5)
 axis(LEFT <-2, at=c(10,30,50,70,90)/4, labels=c("cb", "mz", "mzfre", "mzcb","milpa" ), las= HORIZONTAL<-1, cex.axis=0.9, line=5.5,tick=F)

 axis(LEFT <-2, at=seq(0.5,100.5,4)/4, labels=F, las= HORIZONTAL<-1,
 cex.axis=0.7)
 axis(LEFT <-2, at=seq(2.5, 98.5,4)/4, labels=rep(c("Roundup", "Plaguicida \nHerbicida", "Herbicida", "Desyerbe \nPlaguicida", "Desyerbe" ),5), las= HORIZONTAL<-1, cex.axis=0.85,tick=F)

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
