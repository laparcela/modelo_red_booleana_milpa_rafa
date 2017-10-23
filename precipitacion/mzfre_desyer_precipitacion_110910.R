source("~/Dropbox/Chido/convBase.R")
source("~/Dropbox/Chido/obtAtrGenIte.R")
source("~/Dropbox/Chido/plotAtr.R")

#Definiendo sistema
nodos <-c("Temperatura", "Presion", "Precipitacion", "Herbivoros", "Depredadores", "MaizG", "FrijolEG", "CalabazaG", "Maiz", "FrijolE", "Calabaza", "MaizJ", "CalabazaJ", "Polinizadores", "FloresNoQuelites", "NoQuelites", "FloresQuelites", "Quelites", "FloresBorde", "Borde", "Desyerbe", "Herbicida", "Plaguicida")
#Matriz de adyacencia
matAdya<-matrix(0,length(nodos),length(nodos))
rownames(matAdya)<-nodos
colnames(matAdya)<-nodos
#Definiendo parametros globales
n <-length(nodos)
valu=2
no=3000

manejo="desyer"
diversidad="mzfre"
perturbacion="precipitacion"
nivel="110910"

#Ciclo de simulaciones
lista.repe <-list()
set.seed(1212)
rep=100
seed.rep <-sample(runif(100000,0,999999),rep)

for(r in 1:rep){
	set.seed(seed.rep[r])

	p <-sample(0:(valu^n-1),no,1/valu^n)
	#p <-seq(0,valu^length(nodos)-1,1)#Exploracion exhaustiva
	ei <-convBase(valu,nodos,p)
	rownames(ei) <-nodos
	
	lista.repe[[r]] <-obtAtr(ei,nodos,100,manejo,diversidad,perturbacion,nivel)
}
save(lista.repe,file=paste0("~/Dropbox/Chido/",perturbacion,"/",manejo,"/",diversidad,"/",diversidad,"_",manejo,"_",perturbacion,"_",nivel,".RData"))
