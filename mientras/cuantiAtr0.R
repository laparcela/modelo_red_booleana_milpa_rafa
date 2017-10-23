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

#lista.svdatr<-list()
#lista.long<-list()
#lista.longmasde5<-list()
#lista.mzJ<-list()
#lista.mz<-list()
#lista.mzG<-list()
#lista.fre<-list()
#lista.freG<-list()
#lista.cbJ<-list()
#lista.cb<-list()
#lista.cbG<-list()
lista.prop.mzGJ<-list()
lista.prop.cbGJ<-list()
lista.prop.freGA<-list()
#lista.prop.mzGA<-list()
#lista.prop.cbGA<-list()
lista.prop.quelites<-list()

#lista.svdatr.e<-list()
#lista.long.e<-list()
#lista.longmasde5.e<-list()
#lista.mzJ.e<-list()
#lista.mz.e<-list()
#lista.mzG.e<-list()
#lista.fre.e<-list()
#lista.freG.e<-list()
#lista.cbJ.e<-list()
#lista.cb.e<-list()
#lista.cbG.e<-list()
lista.prop.mzGJ.e<-list()
lista.prop.freGA.e<-list()
lista.prop.cbGJ.e<-list()
#lista.prop.mzGA.e<-list()
#lista.prop.cbGA.e<-list()		
lista.prop.quelites.e<-list()

	if(is.list(x[[1]])){
		for(e in 1:length(x)){
			for(f in 1:length(x[[e]])){#Calculo de los valores singulares de cada atractor. Conjetura: "Mientras mas parecidos los valores singulares de una matriz mas "similares" son las matrices"
#Lema: la descomposicion en valores singulares de una matriz es unica, salvo por aquellas matrices en donde se realizaron permutaciones de filas/columnas o se multiplico a una o mas filas/columnas i.e. cuando una matriz puede expresarse como combinacion lineal de otra(s).
#				lista.svdatr.e[[f]]<-sum(svd(x[[e]][[f]])$d)
#				lista.svdatr[[e]]<-lista.svdatr.e
#				lista.long.e[[f]]<-length(svd(x[[e]][[f]])$d)
#	 			lista.long[[e]]<-lista.long.e
#Si la longitud del vector correspondiente a los valores singulares de la matriz que representa al atractor es mayor o igual que 3 entonces existe la posibilidad de que un temporal se presente y por lo tanto puede realizarse el analisis de si lo que se cultivo se cosecho.	OJO: ESTE CRITERIO NO GARANTIZA QUE EL TEMPORAL SE De... de hecho el temporal podria no darse solo por efecto de la estocasticidad y no por otra razon, en consecuencia puede verse como reacciona el cultivo.
				if(length(svd(x[[e]][[f]])$d)>5){
#					lista.longmasde5.e[[f]]<-length(svd(x[[e]][[f]])$d)
#					lista.longmasde5[[e]]<-lista.longmasde5.e	
#		
#					lista.mzJ.e[[f]]<-sum(x[[e]][[f]]["MaizJ",])
#					lista.mzJ[[e]]<-lista.mzJ.e
#					lista.mz.e[[f]]<-sum(x[[e]][[f]]["Maiz",])
#					lista.mz[[e]]<-lista.mz.e
#					lista.mzG.e[[f]]<-sum(x[[e]][[f]]["MaizG",])
#					lista.mzG[[e]]<-lista.mzG.e
#					lista.fre.e[[f]]<-sum(x[[e]][[f]]["FrijolE",])
#					lista.fre[[e]]<-lista.fre.e
#					lista.freG.e[[f]]<-sum(x[[e]][[f]]["FrijolEG",])
#					lista.freG[[e]]<-lista.freG.e
#					lista.cbJ.e[[f]]<-sum(x[[e]][[f]]["CalabazaJ",])
#					lista.cbJ[[e]]<-lista.cbJ.e
#					lista.cb.e[[f]]<-sum(x[[e]][[f]]["Calabaza",])
#					lista.cb[[e]]<-lista.cb.e
#					lista.cbG.e[[f]]<-sum(x[[e]][[f]]["CalabazaG",])
#					lista.cbG[[e]]<-lista.cbG.e

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
#					if(sum(x[[e]][[f]]["Maiz",])!=0){
#						lista.prop.mzGA.e[[f]]<-(sum(x[[e]][[f]]["MaizG",])/sum(x[[e]][[f]]["Maiz",]))*(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
#						lista.prop.mzGA[[e]]<-lista.prop.mzGA.e
#					}
#					if(sum(x[[e]][[f]]["Maiz",])==0){
#						lista.prop.mzGA.e[[f]]<-0
#						lista.prop.mzGA[[e]]<-lista.prop.mzGA.e
#					}
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
#					if(sum(x[[e]][[f]]["Calabaza",])!=0){
#						lista.prop.cbGA.e[[f]]<-(sum(x[[e]][[f]]["CalabazaG",])/sum(x[[e]][[f]]["Calabaza",]))*(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
#						lista.prop.cbGA[[e]]<-lista.prop.cbGA.e
#					}
#					if(sum(x[[e]][[f]]["Calabaza",])==0){
#						lista.prop.cbGA.e[[f]]<-0
#						lista.prop.cbGA[[e]]<-lista.prop.cbGA.e
#					}
					if(sum(x[[e]][[f]]["Quelites",])!=0){
						lista.prop.quelites.e[[f]]<-((sum(x[[e]][[f]]["Quelites",])+sum(x[[e]][[f]]["FloresQuelites",]))/length(svd(x[[e]][[f]])$d))*(as.numeric(substr((names(x[[e]][f])),1,nchar(names(x[[e]][f]))-1))/(sum(as.numeric(sapply(names(x[[e]][which(lapply(x[[e]],function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
						lista.prop.quelites[[e]]<-lista.prop.quelites.e
					}
					if(sum(x[[e]][[f]]["Quelites",])==0){
						lista.prop.quelites.e[[f]]<-0
						lista.prop.quelites[[e]]<-lista.prop.quelites.e
					}
				}else{
					lista.prop.mzGJ.e[[f]]<-NA
					lista.prop.mzGJ[[e]]<-lista.prop.mzGJ.e
#					lista.prop.mzGA.e[[f]]<-NA
#					lista.prop.mzGA[[e]]<-lista.prop.mzGA.e
					lista.prop.freGA.e[[f]]<-NA
					lista.prop.freGA[[e]]<-lista.prop.freGA.e
					lista.prop.cbGJ.e[[f]]<-NA
					lista.prop.cbGJ[[e]]<-lista.prop.cbGJ.e
#					lista.prop.cbGA.e[[f]]<-NA
#					lista.prop.cbGA[[e]]<-lista.prop.cbGA.e
					lista.prop.quelites.e[[f]]<-NA
					lista.prop.quelites[[e]]<-lista.prop.quelites.e
					next
				}
			}
		}

#MzJ<-sum(unlist(lista.mzJ),na.rm=T)
#Mz<-sum(unlist(lista.mz),na.rm=T)
#MzG<-sum(unlist(lista.mzG),na.rm=T)
#Fre<-sum(unlist(lista.fre),na.rm=T)
#FreG<-sum(unlist(lista.freG),na.rm=T)
#CbJ<-sum(unlist(lista.cbJ),na.rm=T)
#Cb<-sum(unlist(lista.cb),na.rm=T)
#CbG<-sum(unlist(lista.cbG),na.rm=T)

PropPond.MzG_MzJ<-c(mean(sapply(lista.prop.mzGJ,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.mzGJ,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.mzGJ))
#PropPond.MzG_Mz<-c(mean(sapply(lista.prop.mzGA,function(x){sum(unlist(x),na.rm=T)})),sd(sapply(lista.prop.mzGA,function(x){sum(unlist(x),na.rm=T)}))/sqrt(length(lista.prop.mzGA)))
PropPond.FreG_Fre<-c(mean(sapply(lista.prop.freGA,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.freGA,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.freGA))
PropPond.CbG_CbJ<-c(mean(sapply(lista.prop.cbGJ,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.cbGJ,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.cbGJ))
#PropPond.CbG_Cb<-c(mean(sapply(lista.prop.cbGA,function(x){sum(unlist(x),na.rm=T)})),sd(sapply(lista.prop.cbGA,function(x){sum(unlist(x),na.rm=T)}))/sqrt(length(lista.prop.cbGA)))
PropPond.quelites<-c(mean(sapply(lista.prop.quelites,function(x){sum(unlist(x),na.rm=T)})),var(sapply(lista.prop.quelites,function(x){sum(unlist(x),na.rm=T)}))/length(lista.prop.quelites))
shannon<-c(mean(c(PropPond.MzG_MzJ[1],PropPond.FreG_Fre[1],PropPond.CbG_CbJ[1],PropPond.quelites[1])),var(c(PropPond.MzG_MzJ[1],PropPond.FreG_Fre[1],PropPond.CbG_CbJ[1],PropPond.quelites[1])))
#c(mean(sapply(lista.prop.mzGJ,function(x){-1*sum(unlist(x),na.rm=T)*log(sum(unlist(x),na.rm=T)+0.0000000001)})+sapply(lista.prop.freGA,function(x){-1*sum(unlist(x),na.rm=T)*log(sum(unlist(x),na.rm=T)+0.0000000001)})+sapply(lista.prop.cbGJ,function(x){-1*sum(unlist(x),na.rm=T)*log(sum(unlist(x),na.rm=T)+0.0000000001)})+sapply(lista.prop.quelites,function(x){-1*sum(unlist(x),na.rm=T)*log(sum(unlist(x),na.rm=T)+0.0000000001)})),var(sapply(lista.prop.mzGJ,function(x){-1*sum(unlist(x),na.rm=T)*log(sum(unlist(x),na.rm=T)+0.0000000001)})+sapply(lista.prop.freGA,function(x){-1*sum(unlist(x),na.rm=T)*log(sum(unlist(x),na.rm=T)+0.0000000001)})+sapply(lista.prop.cbGJ,function(x){-1*sum(unlist(x),na.rm=T)*log(sum(unlist(x),na.rm=T)+0.0000000001)})+sapply(lista.prop.quelites,function(x){-1*sum(unlist(x),na.rm=T)*log(sum(unlist(x),na.rm=T)+0.0000000001)}))/length(lista.prop.mzGJ))

#odd.MzG_MzJ<-c(mean(sapply(lista.prop.mzGJ,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))})),sd(sapply(lista.prop.mzGJ,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))}))/sqrt(length(lista.prop.mzGJ)))
#odd.MzG_Mz<-c(mean(sapply(lista.prop.mzGA,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))})),sd(sapply(lista.prop.mzGA,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))}))/sqrt(length(lista.prop.mzGA)))
#odd.FreG_Fre<-c(mean(sapply(lista.prop.freGA,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))})),sd(sapply(lista.prop.freGA,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))}))/length(lista.prop.freGA))
#odd.CbG_CbJ<-c(mean(sapply(lista.prop.cbGJ,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))})),sd(sapply(lista.prop.cbGJ,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))}))/sqrt(length(lista.prop.cbGJ)))
#odd.CbG_Cb<-c(mean(sapply(lista.prop.cbGA,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))})),sd(sapply(lista.prop.cbGA,function(x){sum(unlist(x),na.rm=T)/(1-sum(unlist(x),na.rm=T))}))/sqrt(length(lista.prop.cbGA)))

#frec<-list(MzJ,Mz,MzG,Fre,FreG,CbJ,Cb,CbG)
prop<-list(PropPond.MzG_MzJ,PropPond.FreG_Fre,PropPond.CbG_CbJ,PropPond.quelites,shannon)
names(prop)<-c("MzG_MzJ","FreG_Fre","CbG_CbJ","quelites","Shannon")
#prop<-list(PropPond.MzG_MzJ,PropPond.MzG_Mz,PropPond.FreG_Fre,PropPond.CbG_CbJ,PropPond.CbG_Cb)
#odd<-list(odd.MzG_MzJ,odd.MzG_Mz,odd.FreG_Fre,odd.CbG_CbJ,odd.CbG_Cb)
	}else{
		lista.svdatr<-sapply(x,function(x){sum(svd(x)$d)})
	 	lista.long<-sapply(x,function(x){length(svd(x)$d)})
#Si la longitud del vector correspondiente a los valores singulares de la matriz que representa al atractor es mayor o igual que 3 entonces existe la posibilidad de que un temporal se presente y por lo tanto puede realizarse el analisis de si lo que se cultivo se cosecho.OJO: ESTE CRITERIO NO GARANTIZA QUE EL TEMPORAL SE De... de hecho el temporal podria no darse solo por efecto de la estocasticidad y no por otra razon, en consecuencia puede verse como reacciona el cultivo.
		for(e in 1:length(x)){
			if(length(svd(x[[e]])$d)>5){
#				lista.longmasde5[[e]]<-length(svd(x[[e]])$d)
#				lista.mzJ[[e]]<-sum(x[[e]]["MaizJ",])
#				lista.mz[[e]]<-sum(x[[e]]["Maiz",])
#				lista.mzG[[e]]<-sum(x[[e]]["MaizG",])
#				lista.fre[[e]]<-sum(x[[e]]["FrijolE",])
#				lista.freG[[e]]<-sum(x[[e]]["FrijolEG",])
#				lista.cbJ[[e]]<-sum(x[[e]]["CalabazaJ",])
#				lista.cbG[[e]]<-sum(x[[e]]["CalabazaG",])
#				lista.cb[[e]]<-sum(x[[e]]["Calabaza",])

				if(sum(x[[e]]["MaizJ",])!=0){
					lista.prop.mzGJ[[e]]<-sum(x[[e]]["MaizG",])/sum(x[[e]]["MaizJ",])*(as.numeric(substr((names(x[e])),1,nchar(names(x[e]))-1))/(sum(as.numeric(sapply(names(x[which(lapply(x,function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
				}
				if(sum(x[[e]]["MaizJ",])==0){
					lista.prop.mzGJ[[e]]<-0
				}
#				if(sum(x[[e]]["Maiz",])!=0){
#					lista.prop.mzGA[[e]]<-sum(x[[e]]["MaizG",])/sum(x[[e]]["Maiz",])*(as.numeric(substr((names(x[e])),1,nchar(names(x[e]))-1))/(sum(as.numeric(sapply(names(x[which(lapply(x,function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
#				}
#				if(sum(x[[e]]["Maiz",])==0){
#					lista.prop.mzGA[[e]]<-0
#				}
				if(sum(x[[e]]["FrijolE",])!=0){
					lista.prop.freGA[[e]]<-sum(x[[e]]["FrijolEG",])/sum(x[[e]]["FrijolE",])*(as.numeric(substr((names(x[e])),1,nchar(names(x[e]))-1))/(sum(as.numeric(sapply(names(x[which(lapply(x,function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
				}
				if(sum(x[[e]]["FrijolE",])==0){
					lista.prop.freGA[[e]]<-0
				}
				if(sum(x[[e]]["CalabazaJ",])!=0){
					lista.prop.cbGJ[[e]]<-sum(x[[e]]["CalabazaG",])/sum(x[[e]]["CalabazaJ",])*(as.numeric(substr((names(x[e])),1,nchar(names(x[e]))-1))/(sum(as.numeric(sapply(names(x[which(lapply(x,function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
				}
				if(sum(x[[e]]["CalabazaJ",])==0){
					lista.prop.cbGJ[[e]]<-0
				}
#				if(sum(x[[e]]["Calabaza",])!=0){
#					lista.prop.cbGA[[e]]<-sum(x[[e]]["CalabazaG",])/sum(x[[e]]["Calabaza",])*(as.numeric(substr((names(x[e])),1,nchar(names(x[e]))-1))/(sum(as.numeric(sapply(names(x[which(lapply(x,function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
#				}
#				if(sum(x[[e]]["Calabaza",])==0){
#					lista.prop.cbGA[[e]]<-0
#				}
				if(sum(x[[e]]["Quelites",])!=0){
					lista.prop.cbGJ[[e]]<-((sum(x[[e]]["Quelites",])+sum(x[[e]]["FloresQuelites",]))/length(svd(x[[e]]$d)))*(as.numeric(substr((names(x[e])),1,nchar(names(x[e]))-1))/(sum(as.numeric(sapply(names(x[which(lapply(x,function(x){length(svd(x)$d)})>5)]),function(x){(substr(x,1,nchar(x)-1))})))))
				}
				if(sum(x[[e]]["Quelites",])==0){
					lista.prop.cbGJ[[e]]<-0
				}
			}else{
				lista.prop.mzGJ[[e]]<-NA
#				lista.prop.mzGA[[e]]<-NA
				lista.prop.freGA[[e]]<-NA
				lista.prop.cbGJ[[e]]<-NA
#				lista.prop.cbGA[[e]]<-NA
				lista.prop.quelites[[e]]<-NA
				next
			}
		}				

#MzJ<-sum(unlist(lista.mzJ),na.rm=T)
#Mz<-sum(unlist(lista.mz),na.rm=T)
#MzG<-sum(unlist(lista.mzG),na.rm=T)
#Fre<-sum(unlist(lista.fre),na.rm=T)
#FreG<-sum(unlist(lista.freG),na.rm=T)
#CbJ<-sum(unlist(lista.cbJ),na.rm=T)
#Cb<-sum(unlist(lista.cb),na.rm=T)
#CbG<-sum(unlist(lista.cbG),na.rm=T)

PropPond.MzG_MzJ<-c(sum(unlist(lista.prop.mzGJ),na.rm=T),0)
#PropPond.MzG_Mz<-c(sum(unlist(lista.prop.mzGA),na.rm=T),0)
PropPond.FreG_Fre<-c(sum(unlist(lista.prop.freGA),na.rm=T),0)
PropPond.CbG_CbJ<-c(sum(unlist(lista.prop.cbGJ),na.rm=T),0)
#PropPond.CbG_Cb<-c(sum(unlist(lista.prop.cbGA),na.rm=T),0)
PropPond.quelites<-c(sum(unlist(lista.prop.quelites),na.rm=T),0)
shannon<-c(sum(unlist(lista.prop.mzGJ),na.rm=T)*log(sum(unlist(lista.prop.mzGJ),na.rm=T)+0.0000000001)+sum(unlist(lista.prop.freGA),na.rm=T)*log(sum(unlist(lista.prop.freGA),na.rm=T)+0.0000000001)+sum(unlist(lista.prop.cbGJ),na.rm=T)*log(sum(unlist(lista.prop.cbGJ),na.rm=T)+0.0000000001)+sum(unlist(lista.prop.quelites),na.rm=T)*log(sum(unlist(lista.prop.quelites),na.rm=T)+0.0000000001),0)

#odd.MzG_MzJ<-c(sum(unlist(lista.prop.mzGJ),na.rm=T)/(1-sum(unlist(lista.prop.mzGJ),na.rm=T)),0)
#odd.MzG_Mz<-c(sum(unlist(lista.prop.mzGA),na.rm=T)/(1-sum(unlist(lista.prop.mzGA),na.rm=T)),0)
#odd.FreG_Fre<-c(sum(unlist(lista.prop.freGA),na.rm=T)/(1-sum(unlist(lista.prop.freGA),na.rm=T)),0)
#odd.CbG_CbJ<-c(sum(unlist(lista.prop.cbGJ),na.rm=T)/(1-sum(unlist(lista.prop.cbGJ),na.rm=T)),0)
#odd.CbG_Cb<-c(sum(unlist(lista.prop.cbGA),na.rm=T)/(1-sum(unlist(lista.prop.cbGA),na.rm=T)),0)

#frec<-list(MzJ,Mz,MzG,Fre,FreG,CbJ,Cb,CbG)
prop<-list(PropPond.MzG_MzJ,PropPond.FreG_Fre,PropPond.CbG_CbJ,PropPond.quelites,shannon)
names(prop)<-c("MzG_MzJ","FreG_Fre","CbG_CbJ","quelites","Shannon")
#prop<-list(PropPond.MzG_MzJ,PropPond.MzG_Mz,PropPond.FreG_Fre,PropPond.CbG_CbJ,PropPond.CbG_Cb)
#odd<-list(odd.MzG_MzJ,odd.MzG_Mz,odd.FreG_Fre,odd.CbG_CbJ,odd.CbG_Cb)
	}
return(prop)
#frec,prop,odd))
#	
#lista<-list(lista.mzJ, lista.mz, lista.mzG, lista.fre, lista.freG, lista.cbJ, lista.cb, lista.cbG, lista.prop.mzGJ, lista.prop.mzGA, lista.prop.freGA, lista.prop.cbGJ, lista.prop.cbGA, lista.svdatr, lista.long, lista.longmasde5)
#names(lista)<-c("MaizJ","Maiz","MaizG","FrijolE","FrijolEG","CalabazaJ","Calabaza","CalabazaG","MaizG/MaizJ","MaizG/Maiz","CalabazaG/CalabazaJ","CalabazaG/Calabaza","FrijolEG/FrijolE","Frecuencias individuales","Frecuencias por longitud de periodo","Frecuencias por longitud de periodo mayores o iguales a 3")
#,lista))
}
