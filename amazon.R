#Limpiar el archivo#

rm(list=ls())
gc()

#Carga de librerias#

library(lattice)
library(ggplot2)
library(caret)
library(nnet)
library(readxl)
library(readr)
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
library(forecast)

#Carga de datos

amazon2=read_csv2('https://raw.githubusercontent.com/fferrant/excel_files/master/Amazon.csv',col_names=TRUE)
amazon2
amazon2=data.frame(amazon2)
amazon2$quarter
amazon2$revenue_us_m
amazon2$net_income_us_m
amazon2=data.table(amazon2)

#Algunos datos de nuestro dataset

summary(amazon2)
head(amazon2,20)
sum(amazon2$revenue_us_m)
sum(amazon2$net_income_us_m)
max(amazon2$revenue_us_m)
max(amazon2$net_income_us_m)
amazon2[revenue_us_m==max(amazon2$revenue_us_m)]
dim(amazon2)

#Algunos gráficos básico

x11()
hist(amazon2$revenue_us_m,main="Histograma de ventas",ylab="Monto",xlab="Distribución",density = TRUE,col='blue')
plot(amazon2$revenue_us_m~amazon2$net_income_us_m,col='red',xlab="Ganancias",ylab="Ventas")
boxplot(amazon2$revenue_us_m,main="boxplot de ventas")
boxplot(amazon2$net_income_us_m,main="boxplot de ganancias",col="blue")
barplot(amazon2$revenue_us_m,col='green')

# Regresión linea de ventas/revenue

a=c(1,18,32,2345,1919,34,21,22,18,54,57,88,26,99,990,321,611,980,77)
length(a)
a[1]
a[4]
set.seed(48)
particion=createDataPartition(y=amazon2$revenue_us_m,p=0.5,list=FALSE)
train=amazon2[particion,]
test=amazon2[-particion,]
modelo=lm(revenue_us_m~net_income_us_m,data=train)
summary(modelo)
modelo2=lm(revenue_us_m~net_income_us_m-1,data=train)
summary(modelo2)
modelo
modelo2
pred1=predict(modelo,test)
pred1
pred2=predict(modelo2,test)
pred2
test$revenue_us_m
test
er1=sum(abs(test$revenue_us_m-pred1))/sum(test$revenue_us_m)
er1
er2=sum(abs(test$revenue_us_m-pred2))/sum(test$revenue_us_m)
er2
matriz=rbind(c(0,0,0))
matriz
for (i in 1:length(a))
{
  set.seed(a[i])
  particiona=createDataPartition(y=amazon2$revenue_us_m,p=0.5,list=FALSE)
  traina=amazon2[particiona,]
  testa=amazon2[-particiona,]
  modelo3=lm(revenue_us_m~net_income_us_m,data=traina)
  modelo4=lm(revenue_us_m~net_income_us_m-1,data=traina)
  pred3=predict(modelo3,testa)
  pred4=predict(modelo4,testa)
  er3=sum(abs(testa$revenue_us_m-pred3))/sum(testa$revenue_us_m)
  er4=sum(abs(testa$revenue_us_m-pred4))/sum(testa$revenue_us_m)
  vector=c(a[i],er3,er4)
  matriz=rbind(matriz,vector)
}
matriz
matriz=data.frame(matriz)
names(matriz)=c("Semilla","Con_ordenada","Sin_ordenada")
matriz$Semilla
matriz=data.table(matriz)
mean(matriz$Con_ordenada)
mean(matriz$Sin_ordenada)
x11()
plot(matriz$Con_ordenada)
var(matriz$Con_ordenada)
var(matriz$Sin_ordenada)


#Realizar análisis del dataset como una serie de tiempo

amazon3=read.csv("amazon.csv",header=TRUE,sep=";")
head(amazon3)
tiempo=ts(amazon3,start=c(2005,1),frequency=4)
tiempo
window(tiempo,start=c(2011,3),end=c(2015,4))
amazon3=data.frame(amazon3)
names(amazon3)=c("Fecha","Revenue","Income")
tiempo_ventas=ts(amazon3$Revenue,start=c(2005,1),frequency=4)
tiempo_resultados=ts(amazon3$Income,start=c(2005,1),frequency=4)
tiempo_ventas
tiempo_resultados

z=decompose(tiempo_ventas)
x11()
plot(tiempo_ventas)
plot(log(tiempo_ventas))
plot(decompose(tiempo_ventas))
plot(z)

y=decompose(tiempo_resultados)
x11()
plot(tiempo_resultados)
plot(log(tiempo_resultados))
plot(y)

X=log(tiempo_ventas)
diferencial=diff(X)
x11()
plot(diferencial)

D=diff(tiempo_ventas)
x11()
plot(D)
lines(plot(tiempo_ventas))
sin_esta=diff(tiempo_ventas,lag=4)
sin_esta
x11()
plot(sin_esta)
loga=log(tiempo_ventas)
loga
sin_esta2=diff(loga,lag=4)
sin_esta2
x11()
plot(sin_esta2)
auto=auto.arima(window(tiempo_ventas,start=c(2005,1),end=c(2011,4)))
avion=predict(auto,tiempo_ventas)
window(tiempo_ventas,start=c(2013,1),end=c(2019,4))
window(avion$se,start=c(2013,1),end=c(2019,4))
window(tiempo_ventas,start=c(2013,1),end=c(2019,4))-window(avion$se,start=c(2013,1),end=c(2019,4))
sum(window(tiempo_ventas,start=c(2013,1),end=c(2019,4))-window(avion$se,start=c(2013,1),end=c(2019,4)))

matriz=matrix(c(0,0),nrow=1,ncol=2)

