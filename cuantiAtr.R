#Descripcion: funcion que cuantifica las frecuencias de los atractores a los que se llega a partir de una red logica (logica, multivaluada o infinitamente valuada) bajo distintos criterios

cuantiAtr<-function(x){
require(MASS)

svd<-function (x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)#Funcion que obtiene los valores singulares, tomada de la biblioteca "svd" 
{
    x <- as.matrix(x)
    if (any(!is.finite(x))) 
        stop("infinite or missing values in 'x'")
    dx <- dim(x)
    n <- dx[1L]
    p <- dx[2L]
    if (!n || !p) 
        stop("a dimension is zero")
    La.res <- La.svd(x, nu, nv)
    res <- list(d = La.res$d)
    if (nu) 
        res$u <- La.res$u
    if (nv) {
        if (is.complex(x)) 
            res$v <- Conj(t(La.res$vt))
        else res$v <- t(La.res$vt)
    }
    res
}

lista.prop.mzGJ<-list()
lista.prop.cbGJ<-list()
lista.prop.freGA<-list()
lista.prop.quelites<-list()
lista.prop.conj<-list()

lista.prop.mzGJ.e<-list()
lista.prop.freGA.e<-list()
lista.prop.cbGJ.e<-list()		
lista.prop.quelites.e<-list()
lista.prop.conj.e<-list()

		for(e in 1:length(x)){
			for(f in 1:length(x[[e]])){
				if(length(svd(x[[e]][[f]])$d)>5){
					if(sum(x[[e]][[f]]["MaizJ",])!=0){
						lista.prop.mzGJ.e[[f]]<-(sum(x[[e]][[f]]["MaizG",])/sum(x[[e]][[f]]["MaizJ",]))*(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
#*(1/length(x))
#print(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],length)>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
						lista.prop.mzGJ[[e]]<-lista.prop.mzGJ.e

					}
					if(sum(x[[e]][[f]]["MaizJ",])==0){
						lista.prop.mzGJ.e[[f]]<-0
						lista.prop.mzGJ[[e]]<-lista.prop.mzGJ.e
					}
					if(sum(x[[e]][[f]]["FrijolE",])!=0){
						lista.prop.freGA.e[[f]]<-(sum(x[[e]][[f]]["FrijolEG",])/sum(x[[e]][[f]]["FrijolE",]))*(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
						lista.prop.freGA[[e]]<-lista.prop.freGA.e
					}
					if(sum(x[[e]][[f]]["FrijolE",])==0){
						lista.prop.freGA.e[[f]]<-0
						lista.prop.freGA[[e]]<-lista.prop.freGA.e
					}
					if(sum(x[[e]][[f]]["CalabazaJ",])!=0){
						lista.prop.cbGJ.e[[f]]<-(sum(x[[e]][[f]]["CalabazaG",])/sum(x[[e]][[f]]["CalabazaJ",]))*(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
						lista.prop.cbGJ[[e]]<-lista.prop.cbGJ.e
					}						
					if(sum(x[[e]][[f]]["CalabazaJ",])==0){
						lista.prop.cbGJ.e[[f]]<-0
						lista.prop.cbGJ[[e]]<-lista.prop.cbGJ.e
					}
					if(sum(x[[e]][[f]]["Quelites",])!=0){
						lista.prop.quelites.e[[f]]<-(sum(x[[e]][[f]]["Quelites",])/length(svd(x[[e]][[f]])$d))*(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
						lista.prop.quelites[[e]]<-lista.prop.quelites.e
					}
					if(sum(x[[e]][[f]]["Quelites",])==0){
						lista.prop.quelites.e[[f]]<-0
						lista.prop.quelites[[e]]<-lista.prop.quelites.e
					}
					lista.prop.conj.e[[f]]<-((sum(x[[e]][[f]]["MaizG",])+sum(x[[e]][[f]]["FrijolEG",])+sum(x[[e]][[f]]["CalabazaG",])+sum(x[[e]][[f]]["Quelites",]))/(sum(x[[e]][[f]]["MaizJ",])+sum(x[[e]][[f]]["FrijolE",])+sum(x[[e]][[f]]["CalabazaJ",])+length(svd(x[[e]][[f]])$d)))*(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
					lista.prop.conj[[e]]<-lista.prop.conj.e
				}else{
					lista.prop.mzGJ.e[[f]]<-NA
					lista.prop.mzGJ[[e]]<-lista.prop.mzGJ.e
					lista.prop.freGA.e[[f]]<-NA
					lista.prop.freGA[[e]]<-lista.prop.freGA.e
					lista.prop.cbGJ.e[[f]]<-NA
					lista.prop.cbGJ[[e]]<-lista.prop.cbGJ.e
					lista.prop.quelites.e[[f]]<-NA
					lista.prop.quelites[[e]]<-lista.prop.quelites.e
					lista.prop.conj.e[[f]]<-NA
					lista.prop.conj[[e]]<-lista.prop.conj.e
					next
				}
			}
		}


PropPond.MzG_MzJ<-c(mean(sapply(lista.prop.mzGJ,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.mzGJ,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.mzGJ))
PropPond.FreG_Fre<-c(mean(sapply(lista.prop.freGA,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.freGA,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.freGA))
PropPond.CbG_CbJ<-c(mean(sapply(lista.prop.cbGJ,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.cbGJ,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.cbGJ))
PropPond.quelites<-c(mean(sapply(lista.prop.quelites,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.quelites,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.quelites))
PropPond.conj<-c(mean(sapply(lista.prop.conj,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.conj,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.conj))

prop<-list(PropPond.MzG_MzJ,PropPond.FreG_Fre,PropPond.CbG_CbJ,PropPond.quelites,PropPond.conj)
names(prop)<-c("MzG_MzJ","FreG_Fre","CbG_CbJ","quelites","conj")

return(prop)
}
