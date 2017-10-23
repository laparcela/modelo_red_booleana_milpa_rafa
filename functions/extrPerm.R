###########################################
### Function for extracting permanences ###
###########################################

extrPerm<-function(lista.repe){
	lista_permanencias_promedio<-list()
	# Iterating over the independen replicates of each treatment
	for(o in 1:length(lista.repe)){
		x<-lista.repe[[o]]
		# Extracting those attractors which have a period of 6 or greater
		ind_atr6<-as.numeric(unlist(lapply(x,ncol)))>=6
		x<-x[ind_atr6]
		# Total frequencies of (6 or greater period)-attractors
		total<-sum(as.numeric(unlist(lapply(str_extract_all(names(x), pattern="[^%]"), function(x) paste0(x, collapse="")))))
		lista_permanencias<-list()
		for(m in 1:length(x)){	
			atractor<-x[[m]]
			# Jumping frequencies of smaller attractors	
			if(ncol(atractor)<6) next
			
			frec_atr<-as.numeric(paste0(unlist(str_extract_all(names(x[m]),"[^%]")), collapse=""))
			frec_atr<-frec_atr/total
			nodos<-rownames(atractor)
			# Extrayendo juveniles
			mzJ<-grep(as.character(nodos), pattern="^MaizJ$")
			freJ<-grep(as.character(nodos), pattern="^FrijolE$")
			cbJ<-grep(as.character(nodos), pattern="CalabazaJ$")
			quelJ<-grep(as.character(nodos), pattern="^Quelites$")
			ind_cultivosJ<-c(mzJ,freJ,cbJ,quelJ)
			# Extrayendo adultos
			mz<-grep(as.character(nodos), pattern="^MaizG$")
			fre<-grep(as.character(nodos), pattern="^FrijolEG$")
			cb<-grep(as.character(nodos), pattern="CalabazaG$")
			quel<-grep(as.character(nodos), pattern="^FloresQuelites$")
			ind_cultivos<-c(mz,fre,cb,quel)
#			ind_cultivos<-grep(nodos, pattern="(^MaizG$|^FrijolEG$|^CalabazaG$|^FloresQuelites$)")
			# Calculating individual and joint permanences
			totalJ<-apply(atractor[ind_cultivosJ,],1,sum)
			totalA<-apply(atractor[ind_cultivos,],1,sum)

#			conjunta0<-(sum(totalA[-4])/sum(totalJ[-4]))*frec_atr			
			conjunta1<-(sum(totalA)/sum(totalJ))*frec_atr
#			names(conjunta0)<-"conj_cult"
			names(conjunta1)<-"conj_quelit"
			permanencias<-(totalA/totalJ)
			# Verifying if any permanence is Inf or NaN
			permanencias<-ifelse(is.nan(permanencias),NA,permanencias)
			if(any(permanencias>1, na.rm=T)){ print(permanencias);next}
			permanencias<-permanencias*frec_atr
			permanencias<-c(permanencias,conjunta1)			
			lista_permanencias[[m]]<-permanencias
		}
		df_permanencias<-do.call("rbind",lista_permanencias)	
		permanencia_promedio<-tryCatch(apply(df_permanencias,2,sum))
		lista_permanencias_promedio[[o]]<-permanencia_promedio
	}
	df_permanencia_absoluta<-do.call("rbind", lista_permanencias_promedio)
	permanencia_promedio<-apply(df_permanencia_absoluta,2,function(x)mean(na.omit(x)))
	permanencia_mediana<-apply(df_permanencia_absoluta,2,function(x)median(na.omit(x)))
	permanencia_desv_est<-apply(df_permanencia_absoluta,2,function(x)sd(na.omit(x)))
	lista<-list(permanencia_promedio,permanencia_mediana,permanencia_desv_est)
	lista<-lapply(lista,function(x){
			x<-ifelse(x==Inf,NA,x)
			x<-ifelse(is.nan(x),NA,x)
	})
#	}
#	if(is.null(df_permanencia_absoluta))lista<-list(NA,NA,NA)
return(lista)
}
