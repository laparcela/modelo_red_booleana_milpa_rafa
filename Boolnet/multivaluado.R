####Función para generar todos los posibles estados iniciales para redes multivaluadas, i.e. conversor de todos los números desde cero hasta TAMAÑO DEL ESPACIO DE ESTADOS-1 = (número de posibles valores de los estados de los nodos)^(número de nodos de la red)

Conver.Base <-function(b=3,n=6){

	p <-seq(1,b^n-1)
	ei <-matrix(0,b,length(p)+1,byrow=T)
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
