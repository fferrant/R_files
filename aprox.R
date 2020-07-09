#Limpiar el archivo#

rm(list=ls())
gc()

#Carga de librerias#

library(lattice)
library(ggplot2)
library(caret)
library(nnet)
library(readxl)
library(pls)
library(stats)
library(neuralnet)
library(NeuralNetTools)
library(e1071)
library("rpart")
library("data.table")
library("dplyr")
library("rpart.plot")
library("ROCR")
library("randomForest")
library(data.table)
library( "xgboost" )
library( "Matrix" )
library(aplpack)
library(kohonen)
library(crayon)
library(rgl)
library(RPostgreSQL)

#Función para carga de los datasets#

cargar=function(year)
{
  string=paste0("credito-anual-",year,".csv")
  lectura=read.csv2(string,sep=",",encoding = "UTF-8")
  lectura=data.frame(lectura)
  lectura=data.table(lectura)
  return (lectura)
  
}

#Copio y pego los datasets todos uno abajo del otro#

total=data.frame()
for (i in 2009:2019)
{
  total=rbind(total,cargar(i))
}
total=data.table(total)

#Guardo el nuevo dataset en un CSV

write.csv2("Presupuesto_total.csv",x=total)

Encoding()

#Analisis de utilidad de los datos y descarte de los que se consideran no agregan valor

summary(total$ejercicio_presupuestario)
summary(total$ï..impacto_presupuestario_anio)
ftable(total$ejercicio_presupuestario,total$ï..impacto_presupuestario_anio)
total$ï..impacto_presupuestario_anio=NULL
summary(total$sector_id)
total$sector_id=NULL
summary(total$sector_desc)
total$sector_desc=NULL
summary(total$subsector_desc)
total$subsector_desc=NULL
summary(total$subsector_id)
total$subsector_id=NULL
summary(total$caracter_id)
summary(total$caracter_desc)
ftable(total$caracter_id,total$caracter_desc)
total$caracter_id=NULL
summary(total$jurisdiccion_id)
summary(total$jurisdiccion_desc)
unique(total$jurisdiccion_desc)
total$jurisdiccion_id=NULL
length(unique(total$jurisdiccion_desc))
summary(total$subjurisdiccion_id)
ftable(total$subjurisdiccion_id)
ftable(total$subjurisdiccion_id,total$subjurisdiccion_desc)
summary(total$subjurisdiccion_desc)
total$subjurisdiccion_id=NULL
summary(total$entidad_id)
ftable(total$entidad_id)
length(unique(total$entidad_id))
length(unique(total$entidad_desc))
summary(total$entidad_desc)
write.csv2("tablita.csv",x=as.data.frame(ftable(total$entidad_id,total$entidad_desc)))
total$entidad_id=NULL
summary(total$servicio_id)
unique(total$servicio_id)
write.csv2("tablita2.csv",x=as.data.frame(ftable(total$servicio_id,total$servicio_desc)))
total$servicio_id=NULL
write.csv2("tablita3.csv",x=as.data.frame(ftable(total$programa_id,total$programa_desc)))
total$programa_id=NULL
total$subprograma_id=NULL
total$proyecto_id=NULL
total$actividad_id=NULL
summary(total$actividad_desc)
total=data.table(total)
total[actividad_desc=="",]
unique(total$actividad_desc)
total$actividad_desc=NULL
unique(total$obra_desc)
total$obra_id=NULL
total$finalidad_id=NULL
summary(total$finalidad_desc)
total$funcion_id=NULL
summary(total$funcion_desc)
total$inciso_id=NULL
summary(total$inciso_desc)
summary(total$principal_id)
total$principal_id=NULL
summary(total$principal_desc)
total$parcial_id=NULL
total$subparcial_id=NULL
length(unique(total$principal_desc))
summary(total$parcial_desc)
length(unique(total$parcial_desc))
length(unique(total$subparcial_desc))
summary(total$subparcial_desc)
total$subparcial_desc=NULL
summary(total$clasificador_economico_8_digitos_id)
ftable(total$clasificador_economico_8_digitos_id)
total$clasificador_economico_8_digitos_id=NULL
summary(total$clasificador_economico_8_digitos_desc)
total$fuente_financiamiento_id=NULL
summary(total$fuente_financiamiento_desc)
total$ubicacion_geografica_id=NULL
summary(total$ubicacion_geografica_desc)
total$unidad_ejecutora_id=NULL
summary(total$unidad_ejecutora_desc)
length(unique(total$unidad_ejecutora_desc))
total$unidad_ejecutora_desc=NULL
summary(total$credito_presupuestado)
head(total$credito_presupuestado)
head(total)

# Creamos una conexión a postgres

pw="delaburo2012"
drv= dbDriver("PostgreSQL")
con = dbConnect(drv, dbname = "hackaton",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw)
dbWriteTable(con, "Budget2", 
             value = total, append = TRUE, row.names = FALSE)
dbGetQuery(con, "SELECT * from public.\"Budget2\" offset 500000 limit 10;")

sum(total$credito_pagado)

consulta0= dbGetQuery(con, "select ejercicio_presupuestario Anio, sum(credito_comprometido) Comprometido,
           sum(credito_devengado) Devengado, sum(credito_pagado) Pagado
           from public.\"Budget2\"
           group by ejercicio_presupuestario
           order by ejercicio_presupuestario desc;")

typeof(consulta0)
consulta0=data.frame(consulta0)
consulta0$anio
consulta0
sum(consulta0$devengado)

consulta1=dbGetQuery(con,"select ubicacion_geografica_desc Provincia, sum(credito_pagado) Pagado
           from public.\"Budget2\"
           where funcion_desc=\'Judicial\'
           group by ubicacion_geografica_desc 
           order by sum(credito_pagado) desc;")
consulta1=data.frame(consulta1)
consulta1$provincia
