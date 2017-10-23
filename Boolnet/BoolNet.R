####Red booleana mediante BoolNet
##Este script muestra la dinámica estacional obtenida a partir de los nodos Temperatura, Precipitación y Presión mediante una red Booleana y la biblioteca BoolNet.
require(BoolNet)
require(igraph)
red <-loadNetwork("~/RafaBoolNet/est.txt")#Cargando el script que contiene las reglas lógicas de acuerdo con la notación que BoolNet reconoce.
atr <-getAttractors(red)#Obteniendo los atractores mediante actualización sincrónica de las reglas lógicas.
par(ask=T)
plotAttractors(atr)#Plotea los atractores.
