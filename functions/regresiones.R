#########################################################################
### Function for building linear regression for each culture variable ###
#########################################################################
regresiones<-function(db, saturado=F, form="Riqueza+Manejo+Perturbación+Nivel_perturbación", var_resp=c("perm","risk")){

	require(magrittr)
	require(dplyr)
	require(Hmisc)
	require(stringr)

	# Building saturate model
	if(saturado){
		lm(data=df, formula=Permanencia_promedio~Riqueza+Manejo+Perturbación+Nivel_perturbación+Tipo_de_permanencia)->perm	
		lm(data=df, formula=Riesgo_relativo~Riqueza+Manejo+Perturbación+Nivel_perturbación+Tipo_de_permanencia)->risk	
		Lista<-list(perm,risk)
		names(Lista)<-c("Permanencia", "Riesgo")
	}else{
	# Building segmented models
		factores<-unlist(str_split(form,"\\+"))
		if(!any(factores=="Perturbación")){ db%>%select(matches("Perturbación"))%>%lapply(levels)%>%unlist()->pert; pert<-pert[-1] }else{ pert=NULL}
		if(!any(factores=="Tipo_de_permanencia")) db%>%select(matches("Tipo_de_permanencia"))%>%lapply(levels)%>%unlist()->tipo_de_per else tipo_de_per=NULL				

		# Si se harán las regresiones por cultivo por tipo de perturbación
		if(!is.null(pert) & !is.null(tipo_de_per)){
			Lista<-list()
			for(j in 1:length(pert)){
				db %>% filter(Perturbación==pert[j] | Perturbación=="Control")->db1
				lista<-list()
				for(i in 1:length(tipo_de_per)){
					db1%>%filter(Tipo_de_permanencia==tipo_de_per[i])->df
					lm(data=df, formula=as.formula(paste0("Permanencia_promedio~",form)))->perm
					lm(data=df, formula=as.formula(paste0("Riesgo_relativo~",form)))->risk
					if(any(var_resp=="perm") & any(var_resp=="risk")) {lista[[i]]<-list(perm, risk); next}
					if(var_resp=="perm") lista[[i]]<-list(perm)
					if(var_resp=="risk") lista[[i]]<-list(risk)
				}
				names(lista)<-tipo_de_per
				Lista[[j]]<-lista
			}	
			names(Lista)<-pert
		}
		# Si se harán por tipo de perturbación
		if(!is.null(pert) & is.null(tipo_de_per)){
			Lista<-list()
			for(i in 1:length(pert)){
				db %>% filter(Perturbación==pert[i])->df
				lm(data=df, formula=as.formula(paste0("Permanencia_promedio~",form)))->perm
				lm(data=df, formula=as.formula(paste0("Riesgo_relativo~",form)))->risk
				if(any(var_resp=="perm") & any(var_resp=="risk")) {Lista[[i]]<-list(perm, risk); next}
				if(var_resp=="perm") Lista[[i]]<-list(perm)
				if(var_resp=="risk") Lista[[i]]<-list(risk)
			}
			names(Lista)<-pert
		}

		# Si se harán por tipo de permanencia
		if(is.null(pert) & !is.null(tipo_de_per)){
			Lista<-list()
			for(i in 1:length(tipo_de_per)){
				db %>% filter(Tipo_de_permanencia==tipo_de_per[i])->df
				lm(data=df, formula=as.formula(paste0("Permanencia_promedio~",form)))->perm
				lm(data=df, formula=as.formula(paste0("Riesgo_relativo~",form)))->risk
				if(any(var_resp=="perm") & any(var_resp=="risk")) {Lista[[i]]<-list(perm, risk); next}
				if(var_resp=="perm") Lista[[i]]<-list(perm)
				if(var_resp=="risk") Lista[[i]]<-list(risk)
			}
			names(Lista)<-tipo_de_per
		}

	}	
return(Lista)
}
