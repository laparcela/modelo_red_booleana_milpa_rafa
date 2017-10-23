####Función para generar todos los posibles estados iniciales para redes multivaluadas, i.e. conversor de todos los números desde cero hasta n, donde n es el TAMAÑO DEL ESPACIO DE ESTADOS-1 = (número de posibles valores de los estados de los nodos)^(número de nodos de la red)

##Argumentos:
#	b: número de valores que pueden tomar los estados de los nodos o, equivalentemente, base ala que se quieren convertir los números, e.g. b=4, v={0,1,2,3}, para n=20 esto implica un espacio de 2^20 posibles estados.
#OBSERVACIÓN: Pese a que lo anterior se hace buscando tener todo el espacio de estados, esto puede resultar impráctico o inoperable... quizás vale la pena hacer submuestreos del espacio de estados considerando que:
#0. Se conoce el total de estados del espacio de estados y se sabe que cada estado es único, por lo tanto suponiendo que ninguno de los estados predomina sobre otro puede hacerse un muestreo de dicho espacio de estados, PREGUNTA: ¿De qué tamaño debe de ser la muestra tomada para recuperar la dinámica de los atractores a los que se llega?, ¿cuántas muestras deben de tomarse?, ¿puede implementarse algún criterio bayesiano conforme se toma una muestra y se van obteniendo atractores para eficientizar el cómputo?... estas preguntas de momento están en el aire porque aunque se supone una distribución de probabilidad uniforme para seleccionar a los estados iniciales puede ser que nos interese recuperar el total de posibles atractores más que el tamaño de las cuencas que llevan a ellos... al menos de inicio...

#	n: número de elementos/nodos del sistema/red.

##Valor:
#	La función regresa una matriz entera con los estados iniciales como columnas de la misma.
#OBSERVACIÓN:Si no todos los nodos del sistema se quieren  considerar con tal valuación se puede hacer un submuestreo de esta matriz, dejando únicamente comomultivaluados aquellos nodos que sean de interés.

convBase <-function(b=3,nodos,p=NULL){
	
	n=length(nodos)	
	
   if(is.null(p)){
	p    <-seq(1,b^n-1)
	}	
	
	ei <-matrix(0,n,length(p)+1,byrow=FALSE)
	rownames(ei)<-nodos
	
	num.tri <-list()
	for (i in 1:length(p)){
		cociente <-p[i]%/%b
		residuo <-p[i]%%b		
		num.tri[[1]] <-residuo
#		print(paste("Vamos en el número:",p[i]))
#		print(paste("El residuo es:",residuo))
#		print(paste("El cociente es:",cociente))	
		if(cociente == 0){
			ei[1:length(num.tri),i+1] <-unlist(num.tri)
			next
		}else{
			numero = 1
			while(cociente != 0){
				numero <-numero+1
#				print(paste("El cociente aún no es 0 sino",cociente,"va la",numero,"a. división"))	
				residuo <- cociente%%b
				num.tri[[numero]] <-residuo
#				print(paste("El residuo es:",residuo))
				cociente <-cociente%/%b
#				print(paste("El cociente es:",cociente))
				}
		}
		unlist(num.tri)
#		print(unlist(num.tri))	
		ei[1:length(num.tri),i+1] <-unlist(num.tri)
	}
return(ei)
}
