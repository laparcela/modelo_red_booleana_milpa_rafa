#########################################################################
### Function for building linear regression for each culture variable ###
#########################################################################
regreCultivos<-function(db,cultivos=F,anova=T,cor=F,var_resp=c("perm","risk")){

	require(magrittr)
	require(dplyr)
	require(Hmisc)

	if(cultivos==F){
		lm(data=df, formula=Permanencia_promedio~Riqueza+Manejo+Perturbación+Nivel_perturbación+Tipo_de_permanencia)->perm	
		lm(data=df, formula=Riesgo_relativo~Riqueza+Manejo+Perturbación+Nivel_perturbación+Tipo_de_permanencia)->risk	
		lista<-list(perm,risk)
	}
	if(cultivos==T){
		lista<-list()
		cultivos<-levels(db$Tipo_de_permanencia)	
		for(i in 1:length(cultivos)){
			db %>% filter(Tipo_de_permanencia==cultivos[i])->df		
			if(anova==T){
				lm(data=df, formula=Permanencia_promedio~Riqueza+Manejo+Perturbación+Nivel_perturbación)->perm
				lm(data=df, formula=Riesgo_relativo~Riqueza+Manejo+Perturbación+Nivel_perturbación)->risk
				
				if(any(var_resp=="perm") & any(var_resp=="risk")) {lista[[i]]<-list(perm, risk); next}
				if(var_resp=="perm") lista[[i]]<-list(perm)
				if(var_resp=="risk") lista[[i]]<-list(risk)
			}
			if(cor==T){
				df<-na.omit(df)
				lista[[i]]<-cor(df$Permanencia_promedio,df$Riesgo_relativo)
	#			lista[[i]]<-rcorr(cbind(df$Permanencia_promedio,df$Per_desv_est))
			}
		}
	}	
names(lista)<-cultivos
return(lista)
}
