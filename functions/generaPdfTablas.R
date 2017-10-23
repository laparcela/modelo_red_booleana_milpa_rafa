############################################################
### Function for generating a pdf from stargazer outputs ###
############################################################

generaPdfTablas<-function(x=NULL,nombre=NULL, width=8, height=11, columnas=NULL, var_resp=NULL, titulo="",summary=F){
	require(stargazer)
	stargazer(x,out=nombre, column.labels=columnas, dep.var.caption="Variable dependiente",
		  dep.var.labels=var_resp, title=titulo, notes.label="",summary.logical=summary)
	x<-readLines(nombre)
	cat({paste0(
	"\\documentclass[spanish,11pt]{article}\n",
	"\\pdfpagewidth ",width,"in\n",
	"\\pdfpageheight ",height,"in\n",
	"\\usepackage[spanish]{babel}\n",
	"\\selectlanguage{spanish}\n",
	"\\usepackage[utf8]{inputenc}\n",
	"\\begin{document}\n",
	paste0(x,collapse="\n"),
	"\n\\end{document}",
	collapse="\n")},
	file=nombre)
	
#	Sys.sleep(5)
	system(paste0("pdflatex ~/Dropbox/Chido/",nombre))
}
#notes="Los resultados se muestran en el formato usual de los resúmenes para modelos de regresión realizados en R. Estos se leen\\ considerando el tratamiento de referencia (Milpa con desyerbe manual sin perturbaciones) <y observando los coeficientes\\asociados a cada covariable, si el coeficiente es signifi<cativo (p<0.05) entonces el valor correspondiente \\tendrá uno o más *, de no ser así, el coeficiente será igual al valor de referencia"
