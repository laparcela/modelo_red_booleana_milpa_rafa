# Loading libraries
require(stringr)
# Loading functions
source("functions/extrPerm.R")

###############################
### Declarando tratamientos ###
###############################

# Niveles de perturbación
pert<-c("0","110910", "1767", "1434","1212")
prob<-round(c(0,1/10,1/7,1/4,1/2),digits=2)
# Perturbaciones 
#dir0<-c("det", "precipitacion", "arvenses", "herbivoros")
dir0<-c("precipitacion", "arvenses", "herbivoros")
#Perturbaciones<-c("Control", "Sequía", "Arvenses", "Herbívoros")
Perturbaciones<-c("Sequía", "Arvenses", "Herbívoros")
# Esquemas de manejo
dir1<-c("desyer", "desyerPlagui", "herb", "plaguiHerb")#, "Roundup")
Manejos<-c("Desyerbe", "Desyerbe-Insecticida", "Herbicida", "Herbicida-Insecticida")#, "Herbicida_Roundup")
# Esquemas de riqueza
dir2<-c("milpa", "mzfre", "mzcb", "mz", "cb")
Riquezas<-c("Maíz-Frijol-Calabaza","Maíz-Frijol","Maíz-Calabaza","Monocultivo Maíz","Monocultivo Calabaza")
# Cultivos
cultivos<-c("maíz", "frijol", "calabaza", "quelites", "conj_quelit")
Permanencias<-c("Maíz", "Frijol", "Calabaza", "Quelites", "Conjunta" )

#########################################
### Generando dataset de permanencias ###
#########################################
filas<-((length(dir0)-1)*length(dir1)*length(dir2)*length(pert)+length(dir1)*length(dir2))*length(cultivos)
print(filas)
tabla<-matrix(NA,1500,9)
for(k in 1:length(dir0)){
	for(j in 1:length(dir1)){
		for(i in 1:length(dir2)){
#			if(k==1) p=1 else p=length(pert)
#			for(l in 1:p){
			for(l in 1:length(pert)){
				if(l==1){
					ruta<-paste0("det/",dir1[j],"/",dir2[i],"/",dir2[i],"_",dir1[j],"_det_",pert[l],".RData")
				}
				if(l!=1){
					ruta<-paste0(dir0[k],"/",dir1[j],"/",dir2[i],"/",dir2[i],"_",dir1[j],"_",dir0[k],"_",pert[l],".RData")
				}

				archivo<-tryCatch(load(ruta),warning=function(w)1)
				if(is.numeric(archivo)){
					print("no_Esta")
					cat(ruta,sep="\n",append=T,file="tratamientos_faltantes.RData")
					next
				}
				# Extracting/calculating permanences
				lista_permanencias<-extrPerm(lista.repe)
				
				for(n in 1:length(cultivos)){
				# Function for generating matrices

					fila<-(k-1)*length(dir1)*length(dir2)*length(pert)*length(cultivos)+(j-1)*length(dir2)*length(pert)*length(cultivos)+(i-1)*length(pert)*length(cultivos)+(l-1)*length(cultivos)+n
					print(fila)
						# Riqueza
#						tabla[fila,1]<-dir2[i]
						tabla[fila,1]<-Riquezas[i]
						# Manejo
#						tabla[fila,2]<-dir1[j]
						tabla[fila,2]<-Manejos[j]
						# Perturbación
#						tabla[fila,3]<-dir0[k]
						tabla[fila,3]<-Perturbaciones[k]
						# Nivel
#						if(k==1) tabla[fila,4]<-0 else tabla[fila,4]<-prob[l]
						tabla[fila,4]<-prob[l]
						# Cultivos
#						tabla[fila,5]<-cultivos[n]
						tabla[fila,5]<-Permanencias[n]
						tabla[fila,6]<-lista_permanencias[[1]][n]
						tabla[fila,7]<-lista_permanencias[[2]][n]
						tabla[fila,8]<-lista_permanencias[[3]][n]
						tabla[fila,9]<-lista_permanencias[[3]][n]/lista_permanencias[[1]][n]
				}
			}
		}
	}
}
#tabla<-tabla[-(121:480),]
df<-as.data.frame(tabla)
#df$V1<-factor(as.character(df$V1), levels=dir2)
#df$V2<-factor(as.character(df$V2), levels=dir1)
#df$V3<-factor(as.character(df$V3), levels=dir0)
#df$V5<-factor(as.character(df$V5), levels=cultivos)
#df$V5<-factor(as.character(df$V5), levels=cultivos)
df$V1<-factor(as.character(df$V1), levels=Riquezas)
df$V2<-factor(as.character(df$V2), levels=Manejos)
df$V3<-factor(as.character(df$V3), levels=Perturbaciones)
df$V5<-factor(as.character(df$V5), levels=Permanencias)
df$V4<-as.numeric(as.character(df$V4))
df$V6<-as.numeric(as.character(df$V6))
df$V7<-as.numeric(as.character(df$V7))
df$V8<-as.numeric(as.character(df$V8))
df$V9<-as.numeric(as.character(df$V9))
colnames(df)<-c("Riqueza", "Manejo", "Perturbación", "Nivel_perturbación", "Tipo_de_permanencia", "Permanencia_promedio", "Permanencia_mediana", "Riesgo_absoluto","Riesgo_relativo")
 write.csv(df,"datasets/base_datos_permanencias.csv",row.names=F)
