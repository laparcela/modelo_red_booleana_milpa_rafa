# Loading libraries
require(FactoMineR)
require(missMDA)
require(stargazer)
require(dplyr)
require(magrittr)

# Loading hm_functions
source("functions/generaPdfTablas.R")
source("functions/regreCultivos.R")
source("functions/regresiones.R")

# Loading dataset
source("df.R")

# Correlation between permanence and risk
source("making_correlations_perm_risk_by_culture.R")

# Making linear regressions 
lista0<-regresiones(db=df, saturado=T)
lista0_perm<-regresiones(db=df, form="Riqueza+Manejo+Perturbación+Nivel_perturbación", var_resp="perm")
lista0_risk<-regresiones(db=df, form="Riqueza+Manejo+Perturbación+Nivel_perturbación", var_resp="risk")

generaPdfTablas(lista0_perm, "tabla0_perm.tex", width=14, height=13, columnas=c("Individual maíz", "Individual frijol", "Individual calabaza", "Individual quelites","Conjunta" ), var_resp="Permanencia promedio", titulo="Modelo de regresión de la permanencia")
generaPdfTablas(lista0_risk, "tabla0_risk.tex", width=14, height=13, columnas=c("Individual maíz", "Individual frijol", "Individual calabaza", "Individual quelites", "Conjunta" ), var_resp="Riesgo relativo de la permanencia promedio", titulo="Modelo de regresión del riesgo relativo")


#df%>%filter(Perturbación!="Control")->df
# Making exploratory analysis and building graphs
#df<-df[,c(1:3,5,6,7)]



#lista1_perm<-regresiones(db=df, form="Riqueza+Manejo+Tipo_de_permanencia+Nivel_perturbación")
#lista2_perm<-regresiones(db=df, form="Riqueza+Manejo+Nivel_perturbación", var_resp="perm")
#generaPdfTablas(lista0,"tabla_maestra.tex",width=14,height=14)
#generaPdfTablas(lista2_perm[[2]], "tabla_perm02.tex", width=25, height=14, columnas=c("Individual maíz", "Individual frijol", "Individual calabaza", "Conjunta cultivos", "Individual quelites",  "Conjunta cultivos y quelites" ), var_resp="Permanencia promedio", titulo="Modelo de regresión de la permanencia")


#lista_perm<-regreCultivos(df, anova=T, cultivos=T, var_resp="perm")
#lista_risk<-regreCultivos(df, anova=T, cultivos=T, var_resp="risk")
#generaPdfTablas(lista_perm, "tabla_perm01.tex", width=17, height=14, columnas=c("Individual maíz", "Individual frijol", "Individual calabaza", "Conjunta cultivos", "Individual quelites",  "Conjunta cultivos y quelites" ), var_resp="Permanencia promedio", titulo="Modelo de regresión de la permanencia")
#generaPdfTablas(lista_risk, "tabla_risk01.tex", width=17, height=14, columnas=c("Individual maíz", "Individual frijol", "Individual calabaza", "Conjunta cultivos", "Individual quelites",  "Conjunta cultivos y quelites" ), var_resp="Riesgo relativo de la permanencia promedio", titulo="Modelo de regresión del riesgo relativo")
