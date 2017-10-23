# Loading dataset
df<-read.csv("datasets/base_datos_permanencias.csv")
# Niveles de perturbación
#pert<-c("110910", "1767", "1434","1212")
prob<-round(c(1/10,1/7,1/4,1/2),digits=2)
# Perturbaciones 
#dir0<-c("det", "precipitacion", "arvenses", "herbivoros")
Perturbaciones<-c("Control", "Sequía", "Arvenses", "Herbívoros")
# Esquemas de manejo
#dir1<-c("desyer", "desyerPlagui", "herb", "plaguiHerb")#, "Roundup")
Manejos<-c("Desyerbe", "Desyerbe-Insecticida", "Herbicida", "Herbicida-Insecticida")#, "Herbicida_Roundup")
# Esquemas de riqueza
#dir2<-c("milpa", "mzfre", "mzcb", "mz", "cb")
Riquezas<-c("Maíz-Frijol-Calabaza","Maíz-Frijol","Maíz-Calabaza","Monocultivo Maíz","Monocultivo Calabaza")
# Cultivos
#cultivos<-c("maíz", "frijol", "calabaza", "quelites", "conj_cult", "conj_quelit")
Permanencias<-c("Maíz", "Frijol", "Calabaza", "Quelites", "Conjunta" )
# Ordering levels
df$Riqueza<-factor(as.character(df$Riqueza), levels=Riquezas)
df$Manejo<-factor(as.character(df$Manejo), levels=Manejos)
df$Perturbación<-factor(as.character(df$Perturbación), levels=Perturbaciones)
df$Tipo_de_permanencia<-factor(as.character(df$Tipo_de_permanencia), levels=Permanencias)
df<-na.omit(df)
df<-df[,c(1:6,9)]
