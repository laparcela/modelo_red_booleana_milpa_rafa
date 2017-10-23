####ei.Bool{RafaBoolNet}

##Descripción: 
#	Función que genera el conjunto de posibles estados booleanos para un sistema de "n" variables booleanas.

##Argumentos:
#	n: Número de variables booleanas. En teoría n sólo podría valer 32(64).

##Valor:
#	La función devuelve una matriz con el total de estados iniciales booleanos y genera un archivo ei_bool.csv que contiene a los mismos.

ei.Bool <-function(n,nombres=NULL){

	num.ei <-seq(0,(2^n)-1,1)#Número de estados iniciales.
	ei.bit <-sapply(num.ei,function(x){ as.integer(intToBits(x))})#Conversor de base 10 a base 2 para cada elemento de num.ei, genera una matriz de 32(64) filas para un Sistema Operativo de 32(64) bits.
	ei <-ei.bit[1:n,]#Subselección de la matriz de estados iniciales booleanos, ya que dependiendo de n
	rownames(ei) <-nombres
	write.csv(ei,"ei_bool.csv",row.names=FALSE,col.names=FALSE)#Escribe un archivo.csv con los estados iniciales.
	return(ei)#Imprime el espacio de estados iniciales como una matriz en donde cada vector columna es un estado inicial.
}
