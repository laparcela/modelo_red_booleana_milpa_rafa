####Funciones lógicas#####

#Función lógica para presión.
		if(a[1,j-1] == 0) a[3,j]=1 	else { if(a[1,j-1] == 1) a[3,j]=0 }

#Función lógica para temperatura.
		if(a[2,j-1] == 0) a[1,j]=1	else { if(a[2,j-1] == 1) a[1,j]=0 }

#Función lógica para precipitación. 
		if(a[3,j-1] == 0) a[2,j]=1	else { if(a[3,j-1] == 1) a[2,j]=0 }
