obtAtr <-function(ei,nodos,ncol.a=100,manejo=c("desyer", "desyerPlagui", "herb", "plaguiHerb", "Roundup"), diversidad=c("milpa", "mzcb", "mzfre", "mz", "cb"), perturbacion=c("det","precipitacion", "arvenses", "herbivoros"), nivel=c("0","1212", "1434", "1767", "110910")){
#manejo=c("desyer","desyerPlagui","herb","plaguiHerb","Roundup")
#diversidad=c("milpa","mzcb","mzfre","mz","cb")
#perturbacion=c("precipitacion","arvenses","herbivoros")
#nivel=c("1212","1434","1767","110910")
	n <-nrow(ei)#Orden de la red.
	t.ei <-ncol(ei)#
	a <-matrix(0,n,ncol.a)#Matriz para guardar la trayectoria generada en cada iteracion.
	lista.tray <-list()#Lista para guardar las trayectorias.
	lista.atr <-list()#Lista para guardar los atractores.
	cuenca <-rep(1,t.ei)
	rownames(ei)=nodos
	rownames(a)=nodos

	for (i in 1:t.ei){#Este ciclo recorre todos los estados iniciales de la matriz "ei".

###Bloque 1 revisar si el estado inicial es parte de una trayectoria previa.		
		if(i>=2){#Esta compuerta logica evalua si a partir de la segunda iteracion el estado inicial en turno ya se alcanzo en alguna trayectoria previa.
			l.lista.tray<-length(lista.tray)
			for(l in 1:l.lista.tray){
				if(is.null(lista.tray[[l]])){
					cuenca[l]=0
					next
				}else{
					e <-abs(ei[,i]-lista.tray[[l]])#Distancia de Hamming.
					dH.e <-colSums(e)
					dH <-any(dH.e==0)
					if(dH){
						cuenca[l]=cuenca[l]+1
						break
					}
				}
			}		
		}else{
  			dH=F
		}
		if(dH){
#			writeLines(paste("El estado",i,"ya es parte de la trayectoria previa",l))
			next
		}

###Bloque 2 actualizacion de las funciones logicas.
		a[,1]=ei[,i]#Estado inicial.
#		writeLines(paste("Estado inicial:",i))#Mensaje que indica el estado en el que nos encotramos.
#		print(ei[,i,drop=FALSE])#Estado inicial en el que nos encotramos.
           
		for (j in 2:ncol.a){#Iteracion sincronica de las funciones logicas.			
		a<-as.data.frame(t(a))
set.seed(i*j)
per<-sample(0:ncol.a,1)

##Funciones logicas.
#Fijando nodos
#manejo
if(manejo=="desyer"){
	a$Herbicida[j-1]=0
	a$Plaguicida[j-1]=0
}
if(manejo=="desyerPlagui"){
	a$Herbicida[j-1]=0
}
if(manejo=="herb"){
	a$Desyerbe[j-1]=0
	a$Plaguicida[j-1]=0
}
if(manejo=="plaguiHerb"){
	a$Desyerbe[j-1]=0
}
if(manejo=="Roundup"){
	a$Desyerbe[j-1]=0
}
#diversidad
if(diversidad=="mzcb"){
	a$FrijolE[j-1]=0
}
if(diversidad=="mzfre"){
	a$CalabazaJ[j-1]=0
}
if(diversidad=="mz"){
	a$FrijolE[j-1]=0
	a$CalabazaJ[j-1]=0
}
if(diversidad=="cb"){
	a$MaizJ[j-1]=0
	a$FrijolE[j-1]=0
}

#Clima
#set.seed(i)
if(perturbacion=="precipitacion"){
set.seed(per)
	if(nivel=="1212"){ if(a$Presion[j-1]==0) a$Precipitacion[j]=sample(c(0,1),1,p=c(1/2,1/2))}
	if(nivel=="1434"){ if(a$Presion[j-1]==0) a$Precipitacion[j]=sample(c(0,1),1,p=c(1/4,3/4))}
	if(nivel=="1767"){ if(a$Presion[j-1]==0) a$Precipitacion[j]=sample(c(0,1),1,p=c(1/7,6/7))}
	if(nivel=="110910"){ if(a$Presion[j-1]==0) a$Precipitacion[j]=sample(c(0,1),1,p=c(1/10,9/10))}
}
if(perturbacion!="precipitacion"){ 
	if(a$Presion[j-1]==0) a$Precipitacion[j]=1
}
if(a$Presion[j-1]==1) a$Precipitacion[j]=0
if(a$Temperatura[j-1]==0) a$Presion[j]=1 
if(a$Temperatura[j-1]==1) a$Presion[j]=0
if(a$Precipitacion[j-1]==0) a$Temperatura[j]=1	
if(a$Precipitacion[j-1]==1) a$Temperatura[j]=0

#if(i==t.ei) matAdya["Presion","Precipitacion"]=-1
#if(i==t.ei) matAdya["Temperatura","Presion"]=-1
#if(i==t.ei) matAdya["Precipitacion","Temperatura"]=-1

#Borde
if(a$Borde[j-1]==1) a$Borde[j]=1 
if(a$Borde[j-1]==0) a$Borde[j]=0

#if(i==t.ei) matAdya["Borde","Borde"]=1

if(a$Borde[j-1]==1 & a$Presion[j-1]==0) a$FloresBorde[j]=1 
if(a$Borde[j-1]==0 | a$Presion[j-1]==1) a$FloresBorde[j]=0

#if(i==t.ei) matAdya[c("Borde","Presion"),"FloresBorde"]=c(1,-1)

#Milpa
if(a$Presion[j-1]==0 & a$Temperatura[j-1]==1 & a$Precipitacion[j-1]==0) Siembra=1
if(a$Presion[j-1]==1 | a$Temperatura[j-1]==0 | a$Precipitacion[j-1]==1) Siembra=0

if(a$Presion[j-1]==1 & a$Temperatura[j-1]==1 & a$Precipitacion[j-1]==0) Preparar=1
if(a$Presion[j-1]==0 | a$Temperatura[j-1]==0 | a$Precipitacion[j-1]==1) Preparar=0

if((a$Quelites[j-1]==1 | a$NoQuelites[j-1]==1) | Preparar==1){#Herbicida intensivo
#if(Preparar==1){#Herbicida focalizado 
	a$Desyerbe[j]=1
	a$Herbicida[j]=1
}
if((a$Quelites[j-1]==0 & a$NoQuelites[j-1]==0) & Preparar==0){#Herbicida intensivo 
#if(Preparar==0){#Herbicida focalizado 
	a$Desyerbe[j]=0
	a$Herbicida[j]=0
}

#if(i==t.ei){
#	matAdya[c("Quelites","NoQuelites","Presion","Temperatura"),"Desyerbe"]=1
#	matAdya[c("Quelites","NoQuelites","Presion","Temperatura"),"Herbicida"]=1
#	matAdya["Precipitacion",c("Desyerbe","Herbicida")]=-1
#}

if(Siembra==1){
	a$MaizJ[j]=1#Maiz resistente al herbicida o que no es rociado con el
}
if(Siembra==0){
	a$MaizJ[j]=0#Maiz resistente al herbicida o que no es rociado con el
}
if(Siembra==1 & a$Herbicida[j-1]==0){
	a$CalabazaJ[j]=1
}
if(Siembra==0 | a$Herbicida[j-1]==1){
	a$CalabazaJ[j]=0
}

#if(i==t.ei){
#	matAdya["Temperatura",c("MaizJ","CalabazaJ")]=1
#	matAdya[c("Precipitacion","Presion"),"MaizJ"]=-1
#	matAdya[c("Precipitacion","Presion","Herbicida"),"CalabazaJ"]=-1
#}

#Cultivos adultos
if(a$MaizJ[j-1]==1 & (a$Quelites[j-1]==0 & a$NoQuelites[j-1]==0) & a$Precipitacion[j-1]==1) a$Maiz[j]=1
if(a$MaizJ[j-1]==0 | (a$Quelites[j-1]==1 | a$NoQuelites[j-1]==1) | a$Precipitacion[j-1]==0) a$Maiz[j]=0

if(manejo=="Roundup"){
	if(a$MaizJ[j-1]==1 & a$Precipitacion[j-1]==1 & a$Herbicida[j-1]==0) a$FrijolE[j]=1
	if(a$MaizJ[j-1]==0 | a$Precipitacion[j-1]==0 | a$Herbicida[j-1]==1) a$FrijolE[j]=0
}
if(manejo!="Roundup"){
	if(a$MaizJ[j-1]==1 & a$Precipitacion[j-1]==1) a$FrijolE[j]=1
	if(a$MaizJ[j-1]==0 | a$Precipitacion[j-1]==0) a$FrijolE[j]=0
}

if(a$CalabazaJ[j-1]==1 & (a$Quelites[j-1]==0 & a$NoQuelites[j-1]==0) & a$Precipitacion[j-1]==1 & a$Herbicida[j-1]==0) a$Calabaza[j]=1
if(a$CalabazaJ[j-1]==0 | (a$Quelites[j-1]==1 | a$NoQuelites[j-1]==1) | a$Precipitacion[j-1]==0 | a$Herbicida[j-1]==1) a$Calabaza[j]=0

#if(i==t.ei){
#	matAdya[c("MaizJ","Precipitacion"),"Maiz"]=1
#	matAdya[c("MaizJ","Precipitacion"),"FrijolE"]=1
#	matAdya[c("CalabazaJ","Precipitacion"),"Calabaza"]=1#Revisar precipitación, puede ser que no sea necesaria
#	matAdya[c("Quelites","NoQuelites"),"Maiz"]=-1
#	matAdya[c("Herbicida"),"FrijolE"]=-1
#	matAdya[c("Quelites","NoQuelites","Herbicida"),"Calabaza"]=-1
#}

#Granos
if(a$Maiz[j-1]==1 & a$Precipitacion[j-1]==1 & (a$Herbivoros[j-1]==0 | a$Depredadores[j-1]==1)) a$MaizG[j]=1
if(a$Maiz[j-1]==0 | a$Precipitacion[j-1]==0 | (a$Herbivoros[j-1]==1 & a$Depredadores[j-1]==0)) a$MaizG[j]=0

if(a$Maiz[j-1]==1 & a$FrijolE[j-1]==1 & a$Precipitacion[j-1]==1 & (a$Herbivoros[j-1]==0 | a$Depredadores[j-1]==1)) a$FrijolEG[j]=1
if(a$Maiz[j-1]==0 | a$FrijolE[j-1]==0 | a$Precipitacion[j-1]==0 | (a$Herbivoros[j-1]==1 & a$Depredadores[j-1]==0)) a$FrijolEG[j]=0

if(a$Calabaza[j-1]==1 & a$Presion[j-1]==0 & a$Polinizadores[j-1]==1 & a$Herbicida[j-1]==0 & (a$Herbivoros[j-1]==0 | a$Depredadores[j-1]==1)) a$CalabazaG[j]=1
if(a$Calabaza[j-1]==0 | a$Presion[j-1]==1 | a$Polinizadores[j-1]==0 | a$Herbicida[j-1]==1 | (a$Herbivoros[j-1]==1 & a$Depredadores[j-1]==0)) a$CalabazaG[j]=0	

#if(i==t.ei){
#	matAdya[c("Maiz","Precipitacion","Depredadores"),"MaizG"]=1
#	matAdya[c("FrijolE","Precipitacion","Depredadores"),"FrijolEG"]=1
#	matAdya[c("Calabaza","Polinizadores","Depredadores"),"CalabazaG"]=1
#	matAdya["Herbivoros",c("MaizG","FrijolEG","CalabazaG")]=-1
#	matAdya["Presion","CalabazaG"]=-1
#}

#Interior
if(a$Herbicida[j-1]==0 & a$Desyerbe[j-1]==0 & a$Calabaza[j-1]==0) a$NoQuelites[j]=1
if(perturbacion=="arvenses"){
set.seed(per)
	if(nivel=="1212"){
		if(a$Herbicida[j-1]==1 | a$Desyerbe[j-1]==1 | a$Calabaza[j-1]==1) a$NoQuelites[j]=sample(c(1,0),1,p=c(1/2,1/2))}
	if(nivel=="1434"){
		if(a$Herbicida[j-1]==1 | a$Desyerbe[j-1]==1 | a$Calabaza[j-1]==1) a$NoQuelites[j]=sample(c(1,0),1,p=c(1/4,3/4))}
	if(nivel=="1767"){
		if(a$Herbicida[j-1]==1 | a$Desyerbe[j-1]==1 | a$Calabaza[j-1]==1) a$NoQuelites[j]=sample(c(1,0),1,p=c(1/7,6/7))}
	if(nivel=="110910"){ if(a$Herbicida[j-1]==1 | a$Desyerbe[j-1]==1 | a$Calabaza[j-1]==1) a$NoQuelites[j]=sample(c(1,0),1,p=c(1/10,9/10))}
}
if(perturbacion!="arvenses"){ 
	if(a$Herbicida[j-1]==1 | a$Desyerbe[j-1]==1 | a$Calabaza[j-1]==1) a$NoQuelites[j]=0
}

#if(i==t.ei) matAdya[c("Herbicida","Desyerbe","Calabaza"),"NoQuelites"]=-1

if(a$Herbicida[j-1]==0 & a$Calabaza[j-1]==0 & Siembra==0) a$Quelites[j]=1
if(perturbacion=="arvenses"){
set.seed(per+1)
	if(nivel=="1212"){
	 	if(a$Herbicida[j-1]==1 | a$Calabaza[j-1]==1 | Siembra==1) a$Quelites[j]=sample(c(1,0),1,p=c(1/2,1/2))}
	if(nivel=="1434"){ 	
		if(a$Herbicida[j-1]==1 | a$Calabaza[j-1]==1 | Siembra==1) a$Quelites[j]=sample(c(1,0),1,p=c(1/4,3/4))}
	if(nivel=="1767"){
	 	if(a$Herbicida[j-1]==1 | a$Calabaza[j-1]==1 | Siembra==1) a$Quelites[j]=sample(c(1,0),1,p=c(1/7,6/7))}
	if(nivel=="110910"){
	 	if(a$Herbicida[j-1]==1 | a$Calabaza[j-1]==1 | Siembra==1) a$Quelites[j]=sample(c(1,0),1,p=c(1/10,9/10))}
}
if(perturbacion!="arvenses"){ 
	if(a$Herbicida[j-1]==1 | a$Calabaza[j-1]==1 | Siembra==1) a$Quelites[j]=0
}

#if(i==t.ei) matAdya[c("Precipitacion","Presion"),"Quelites"]=1
#if(i==t.ei) matAdya[c("Herbicida","Calabaza","Temperatura"),"Quelites"]=-1

if(a$NoQuelites[j-1]==1 & a$Precipitacion[j-1]==1 & a$Herbicida[j-1]==0 & a$Desyerbe[j-1]==0) a$FloresNoQuelites[j]=1 
if(a$NoQuelites[j-1]==0 | a$Precipitacion[j-1]==0 | a$Herbicida[j-1]==1 | a$Desyerbe[j-1]==1) a$FloresNoQuelites[j]=0

#if(i==t.ei) matAdya[c("NoQuelites","Precipitacion"),"FloresNoQuelites"]=1
#if(i==t.ei) matAdya[c("Herbicida","Desyerbe"),"FloresNoQuelites"]=-1

if(a$Quelites[j-1]==1 & a$Precipitacion[j-1]==1 & a$Herbicida[j-1]==0) a$FloresQuelites[j]=1 
if(a$Quelites[j-1]==0 | a$Precipitacion[j-1]==0 | a$Herbicida[j-1]==1) a$FloresQuelites[j]=0

#if(i==t.ei) matAdya[c("Quelites","Precipitacion"),"FloresQuelites"]=1
#if(i==t.ei) matAdya["Herbicida","FloresQuelites"]=-1

#Herbivoros
if(a$Maiz[j-1]==1 | a$MaizJ[j-1]==1 | a$MaizG[j-1]==1) Maiz=1 
if(a$Maiz[j-1]==0 & a$MaizJ[j-1]==0 & a$MaizG[j-1]==0) Maiz=0

if(a$FrijolE[j-1]==1 | a$FrijolEG[j-1]==1) FrijolE=1
if(a$FrijolE[j-1]==0 & a$FrijolEG[j-1]==0) FrijolE=0

if(a$CalabazaJ[j-1]==1 | a$Calabaza[j-1]==1 | a$CalabazaG[j-1]==1) Calabaza=1
if(a$CalabazaJ[j-1]==0 & a$Calabaza[j-1]==0 & a$CalabazaG[j-1]==0) Calabaza=0

if((Maiz==1 | FrijolE==1 | Calabaza==1 | a$Quelites[j-1]==1 | a$NoQuelites[j-1]==1 | a$FloresQuelites[j-1]==1 | a$FloresNoQuelites[j-1]==1) & a$Depredadores[j-1]==0 & a$Plaguicida[j-1]==0) a$Herbivoros[j]=1
if(perturbacion=="herbivoros"){
set.seed(per)
	if(nivel=="1212"){ 
		if((Maiz==0 & FrijolE==0 & Calabaza==0 & a$Quelites[j-1]==0 & a$NoQuelites[j-1]==0 & a$FloresQuelites[j-1]==1 & a$FloresNoQuelites[j-1]==1) | a$Depredadores[j-1]==1 | a$Plaguicida[j-1]==1) a$Herbivoros[j]=sample(c(1,0),1,p=c(1/2,1/2))}
	if(nivel=="1434"){ 
		if((Maiz==0 & FrijolE==0 & Calabaza==0 & a$Quelites[j-1]==0 & a$NoQuelites[j-1]==0 & a$FloresQuelites[j-1]==1 & a$FloresNoQuelites[j-1]==1) | a$Depredadores[j-1]==1 | a$Plaguicida[j-1]==1) a$Herbivoros[j]=sample(c(1,0),1,p=c(1/4,3/4))}
	if(nivel=="1767"){ if((Maiz==0 & FrijolE==0 & Calabaza==0 & a$Quelites[j-1]==0 & a$NoQuelites[j-1]==0 & a$FloresQuelites[j-1]==1 & a$FloresNoQuelites[j-1]==1) | a$Depredadores[j-1]==1 | a$Plaguicida[j-1]==1) a$Herbivoros[j]=sample(c(1,0),1,p=c(1/7,6/7))}
	if(nivel=="110910"){ if((Maiz==0 & FrijolE==0 & Calabaza==0 & a$Quelites[j-1]==0 & a$NoQuelites[j-1]==0 & a$FloresQuelites[j-1]==1 & a$FloresNoQuelites[j-1]==1) | a$Depredadores[j-1]==1 | a$Plaguicida[j-1]==1) a$Herbivoros[j]=sample(c(1,0),1,p=c(1/10,9/10))}
}
if(perturbacion!="herbivoros"){ 
	if((Maiz==0 & FrijolE==0 & Calabaza==0 & a$Quelites[j-1]==0 & a$NoQuelites[j-1]==0 & a$FloresQuelites[j-1]==1 & a$FloresNoQuelites[j-1]==1) | a$Depredadores[j-1]==1 | a$Plaguicida[j-1]==1) a$Herbivoros[j]=0
}

#if(i==t.ei){
#	matAdya[c("MaizJ","MaizG","Maiz","FrijolE","FrijolEG","CalabazaJ",
#"Calabaza","CalabazaG","Quelites","NoQuelites","FloresQuelites","FloresNoQuelites"),"Herbivoros"]=1
#	matAdya[c("Depredadores","Plaguicida"),"Herbivoros"]=-1
#}

if(a$Herbivoros[j-1]==1) a$Plaguicida[j]=1#Plaguicida intensivo
if(a$Herbivoros[j-1]==0) a$Plaguicida[j]=0#Plaguicida intensivo

#if(i==t.ei) matAdya[c("Herbivoros"),"Plaguicida"]=1
#if(a$Herbivoros[j-1]==1 & (Maiz==1 | FrijolE==1 | Calabaza==1)) a$Plaguicida[j]=1#Plaguicida focalizado
#if(a$Herbivoros[j-1]==0 | (Maiz==0 & FrijolE==0 & Calabaza==0)) a$Plaguicida[j]=0#Plaguicida focalizado

#Depredadores
if(a$Herbivoros[j-1]==1 & a$Borde[j-1]==1 & a$Plaguicida[j-1]==0) a$Depredadores[j]=1 
if(a$Herbivoros[j-1]==0 | a$Borde[j-1]==0 | a$Plaguicida[j-1]==1) a$Depredadores[j]=0

#if(i==t.ei){
#	matAdya[c("Herbivoros","Borde"),"Depredadores"]=1
#	matAdya["Plaguicida","Depredadores"]=-1
#}

#Polinizadores
if((a$Calabaza[j-1]==1 | a$FloresBorde[j-1]==1 | a$FloresQuelites[j-1]==1 | a$FloresNoQuelites[j-1]==1) & a$Plaguicida[j-1]==0) a$Polinizadores[j]=1 
if((a$Calabaza[j-1]==0 & a$FloresBorde[j-1]==0 & a$FloresQuelites[j-1]==0 & a$FloresNoQuelites[j-1]==0) | a$Plaguicida[j-1]==1) a$Polinizadores[j]=0

#if(i==t.ei){
#	matAdya[c("Calabaza","FloresBorde","FloresNoQuelites","FloresQuelites"),"Polinizadores"]=1
#	matAdya["Plaguicida","Polinizadores"]=-1
#}

#Fijando nodos
if(manejo=="desyer"){
	a$Herbicida[j]=0
	a$Plaguicida[j]=0
}
if(manejo=="desyerPlagui"){
	a$Herbicida[j]=0
}
if(manejo=="herb"){
	a$Desyerbe[j]=0
	a$Plaguicida[j]=0
}
if(manejo=="plaguiHerb"){
	a$Desyerbe[j]=0
}
if(manejo=="Roundup"){
	a$Desyerbe[j]=0
}
#diversidad
if(diversidad=="mzcb"){
	a$FrijolE[j]=0
}
if(diversidad=="mzfre"){
	a$CalabazaJ[j]=0
}
if(diversidad=="mz"){
	a$FrijolE[j]=0
	a$CalabazaJ[j]=0
}
if(diversidad=="cb"){
	a$MaizJ[j]=0
	a$FrijolE[j]=0
}

		a<-as.matrix(a)
		a<-t(a)
# 		print(a)

###Bloque 3 detector de atractores.
			b <- abs(a[,j]-a[,1:(j-1)])
			if(is.null(dim(b))){
				dh.b <-sum(b)
			}else{
				dh.b <-colSums(b)
			}			
			dh <-any(dh.b==0)	
			if(dh){
#					writeLines(paste("Se alcanzo el atractor en la iteracion:",k+1))
#			   	writeLines(paste("El periodo del atractor es de:",k))
#					print(a[,(j-k):(j-1)])#Imprime el atractor al que se llego.
				k <-min(which(dh.b==0))
				if(i>=2){
					l.lista.atr <-length(lista.atr)
					for(o in 1:l.lista.atr){
						if(is.null(lista.atr[[o]])){
							next
						}
						if(is.null(dim(lista.atr[[o]]))){
							q <-abs(lista.atr[[o]]-a[,k:(j-1)])
							if(is.null(dim(q))){
									Dh.q <-sum(q)	
								}else{
									Dh.q <-colSums(q)
								}
								Dh <-any(Dh.q==0)
								if(Dh){
#									writeLines(paste("El atractor proveniente del estado",i,"ya se habia guardado en la posicion",o))	
									cuenca[o]=cuenca[o]+1
									break
								}
						}else{
							ncol.atr <-ncol(lista.atr[[o]])
							for(p in 1:ncol.atr){
								q <-abs(lista.atr[[o]][,p]-a[,k:(j-1)])
								if(is.null(dim(q))){
									Dh.q <-sum(q)	
								}else{
									Dh.q <-colSums(q)
								}
								Dh <-any(Dh.q==0)
								if(Dh){
#									writeLines(paste("El atractor proveniente del estado",i,"ya se habia guardado en la posicion",o))	
									cuenca[o]=cuenca[o]+1
									break
								}
							}
						}
						if(Dh){
							break
						}
					}
					if(Dh){
						break
					}
				}
				lista.atr[[i]] <-a[,k:(j-1)]
				lista.tray[[i]] <-a
#				print(a)
				break
			}		
		}	
	}		
cuenca <-cuenca[1:length(lista.atr)]
cuenca <-round(cuenca/sum(cuenca)*100,2)
names(lista.atr) <-paste(cuenca,"%",sep="")
lista.atr<-lista.atr[!sapply(lista.atr,is.null)]
#resultados<-c(manejo,diversidad,perturbacion,nivel)
#names(resultados)<-c("manejo","diversidad","perturbacion","nivel")
#return(c(lista.atr,resultados))
return(lista.atr)
#write.csv(matAdya,"~/Dropbox/Chido/Control/matriz_adyacencia_milpa_trans.csv")#Matriz de adyacencia
#return(lista.tray)
}

