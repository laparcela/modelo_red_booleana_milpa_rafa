# Loading libraries
require(FactoMineR)
require(missMDA)
require(stargazer)
require(dplyr)
require(magrittr)
# Correlation between permanence and risk by culture
source("df.R")
#perm_risk<-df[,c(6,9)]
correlaciones<-PCA(df,quanti.sup=c(4), quali.sup=c(1:3,5),graph=F)
png(paste0("cor_per_risk_.png"), height=6, width=6, units="in", res=300)
par(cex=0.8)
	plot.PCA(correlaciones, choix="var", axes=c(1,2), invisible=c("ind"), title=paste0("Correlación entre la permanencia promedio y el riesgo relativo"))

nivel<-levels(df$Tipo_de_permanencia)
#for(i in 1:length(nivel)){
#df%>%filter(Tipo_de_permanencia==nivel[i])->db
#db<-db[,c(6,7)]
#correlaciones<-PCA(db,graph=F)
#	plot.PCA(correlaciones, choix="var", axes=c(1,2), lab.ind=F, title=paste0("Correlación entre la permanencia promedio y el riesgo relativo\n(",nivel[i],")"))
#}
dev.off()

titulos=c("a)","b)","c)","d)","e)")
for(i in 1:length(nivel)){
df%>%filter(Tipo_de_permanencia==nivel[i])->db
#m<-db[,(-5)]
analisis<-FAMD(db, sup.var=4, graph=F)

#pdf("var_cuanti_0.pdf")
#	plot.FAMD(analisis, choix="quanti", axes=c(1,2), lab.ind=F, title="Gráfico de variables cuantitativas")
#dev.off()
#pdf("var_cuali_0.pdf")
#	plot.FAMD(analisis, choix="quali", axes=c(1,2), lab.ind=F, title="Gráfico de variables categóricas")
#dev.off()
#pdf("var_cuali_cuanti_0.pdf")
#	plot.FAMD(analisis, choix="var", axes=c(1,2), lab.ind=F, title="Gráfico de variables")
#dev.off()

png(paste0("mapa_obs_var_",nivel[i],".png"),width=8,height=11, units="in", res=300)
par(mfrow=c(3,2), cex=0.85)
	plot.FAMD(analisis, choix="quanti", axes=c(1,2), lab.ind=F, title="a)")

#pdf("mapa_obs_var_perm.pdf")
if(i!=4)plot.FAMD(analisis, choix="ind", axes=c(1,2), lab.var=F, lab.ind=F, habillage="Tipo_de_permanencia", title="b)")#, invisible=c("ind","ind.sup"))
if(i==4)plot.FAMD(analisis, choix="ind", axes=c(1,2), lab.var=F, lab.ind=F, habillage="Tipo_de_permanencia", title="b)",legend=list(x="topright"))#, invisible=c("ind","ind.sup"))

	abline(a=0,b=1,col="blue")
	abline(a=0,b=-1,col="red")
	if(i!=4){
		text(1,1,"+P",col="red")
		text(-1,-1,"-P",col="red")
		text(-1,1,"+R",col="blue")
		text(1,-1,"-R",col="blue")
	}
	if(i==4){
		text(1,1,"-P",col="red")
		text(-1,-1,"+P",col="red")
		text(-1,1,"+R",col="blue")
		text(1,-1,"-R",col="blue")
	}

if(i!=4)plot.FAMD(analisis, choix="ind", axes=c(1,2), lab.var=F, lab.ind=F, habillage="Perturbación", title="c)")#, invisible=c("ind","ind.sup"))
if(i==4)plot.FAMD(analisis, choix="ind", axes=c(1,2), lab.var=F, lab.ind=F, habillage="Perturbación", title="c)", legend=list(x="topright"))
	abline(a=0,b=1,col="blue")
	abline(a=0,b=-1,col="red")
	if(i!=4){
		text(1,1,"+P",col="red")
		text(-1,-1,"-P",col="red")
		text(-1,1,"+R",col="blue")
		text(1,-1,"-R",col="blue")
	}
	if(i==4){
		text(1,1,"-P",col="red")
		text(-1,-1,"+P",col="red")
		text(-1,1,"+R",col="blue")
		text(1,-1,"-R",col="blue")
	}

if(i!=4)plot.FAMD(analisis, choix="ind", axes=c(1,2), lab.var=F, lab.ind=F, habillage="Manejo", title="d)")#, invisible=c("ind","ind.sup"))
if(i==4)plot.FAMD(analisis, choix="ind", axes=c(1,2), lab.var=F, lab.ind=F, habillage="Manejo", title="d)", legend=list(x="topright"))
	abline(a=0,b=1,col="blue")
	abline(a=0,b=-1,col="red")
	if(i!=4){
		text(1,1,"+P",col="red")
		text(-1,-1,"-P",col="red")
		text(-1,1,"+R",col="blue")
		text(1,-1,"-R",col="blue")
	}
	if(i==4){
		text(1,1,"-P",col="red")
		text(-1,-1,"+P",col="red")
		text(-1,1,"+R",col="blue")
		text(1,-1,"-R",col="blue")
	}

#dev.off()
#pdf("mapa_obs_var_riqueza.pdf")
if(i!=4)	plot.FAMD(analisis, choix="ind", axes=c(1,2), lab.var=F,lab.ind=F, habillage="Riqueza", title="e)")#, invisible=c("ind","ind.sup"))
if(i==4)plot.FAMD(analisis, choix="ind", axes=c(1,2), lab.var=F,lab.ind=F, habillage="Riqueza", title="e)", legend=list(x="topright"))
	abline(a=0,b=1,col="blue")
	abline(a=0,b=-1,col="red")
	if(i!=4){
		text(1,1,"+P",col="red")
		text(-1,-1,"-P",col="red")
		text(-1,1,"+R",col="blue")
		text(1,-1,"-R",col="blue")
	}
	if(i==4){
		text(1,1,"-P",col="red")
		text(-1,-1,"+P",col="red")
		text(-1,1,"+R",col="blue")
		text(1,-1,"-R",col="blue")
	}

dev.off()
}
