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



#Carga de datos#

setwd("~/RETAIL")
hapiness=read.csv2("hapiness.csv",header=TRUE)
attach(hapiness)
hapiness2=hapiness
hapiness3=hapiness
hapiness3$Social.support=NULL
hapiness3$Country.name=NULL
hapiness3$Healthy.life.expectancy=NULL

#Obteniendo algunos parámetros básicos#

head(hapiness)
summary(hapiness)
hapiness
hapiness3
hapiness[0:30,]
dim(hapiness)
Social.support
summary(Social.support)
names(hapiness)

#Realizando algunos gráficos generales#

x11()
pairs(hapiness)
boxplot(hapiness3,col=rainbow(10))
plot(hapiness3$Generosity~hapiness3$Perceptions.of.corruption)
hist(hapiness$Logged.GDP.per.capita,,col='green',main = 'Histograma')
plot(hapiness$Ladder.score~hapiness$Logged.GDP.per.capita,col=hapiness$Regional.indicator,xlab='GDP',ylab = 'Score',main='Clasif')


#Análisis de regresión simple#

simple=lm(Ladder.score~Freedom.to.make.life.choices,data=hapiness)
simple$coefficients
simple$residuals
summary(simple)
summary(Ladder.score)
x11()
plot(Freedom.to.make.life.choices,Ladder.score)
abline(simple$coef, col= "red",pch=80)
abline(coef = coef(simple))
simple2=lm(Ladder.score~Healthy.life.expectancy,data=hapiness)
summary(simple2)
x11()
plot(hapiness$Healthy.life.expectancy,hapiness$Ladder.score)
abline(simple2)
vector=c(45,91,38,66,65,63,71,50,80,75,42)
vector
testear=data.frame(vector)
testear2=data.frame(Healthy.life.expectancy=c(73))
data(testear2)
testear
su=predict(simple2,testear2)
su

#Análisis de regresión múltiple#

hapiness2$Country.name=NULL
hapiness2$Regional.indicator=NULL
hapiness2$Ladder.score=NULL
multiple=lm(Healthy.life.expectancy~.,data=hapiness2)
summary(multiple)
recorte=data.frame(Freedom.to.make.life.choices,Social.support,Healthy.life.expectancy,Generosity)
multiple2=lm(Healthy.life.expectancy~Freedom.to.make.life.choices+Social.support,data=recorte)
summary(multiple2)


#Reducción del dataframe a componentes principales#

componentes=prcomp(hapiness[,3:20])
x11()
plot(componentes,type="b")
plot(componentes,type="l")
componentes$sdev
summary(componentes)
x11()
biplot(componentes,pc.biplot=TRUE,cex=0.4)

#Cálculo de distancia de mahalanobis#

para_mahala=data.frame(Healthy.life.expectancy,Explained.by..Generosity)
summary(para_mahala)
mahala=mahalanobis(para_mahala , colMeans(para_mahala), cov(para_mahala))
summary(mahala)
sort(mahala)
x11()
plot(mahala)


#Construir una red neuronal para clasificación#

hapiness4=hapiness
hapiness4$Country.name=NULL
party=createDataPartition(y=hapiness4$Regional.indicator,p=0.7,list=FALSE)
train2=hapiness4[party,]
test2=hapiness4[-party,]
set.seed(42)
red2=nnet(Regional.indicator~.,train2,size=30,maxit=10000,linout=FALSE,MaxNWts=10000)
summary(red2)
predecir=predict(red2,test2,type="class")
predecir
conf=confusionMatrix(as.factor(predecir),test2$Regional.indicator)
conf$overall[1]
x11()
plotnet(red2)

#Construir una red neuronal para regresión#

recorte
division=createDataPartition(y=recorte$Healthy.life.expectancy,p=0.7,list=FALSE)
train3=recorte[division,]
test3=recorte[-division,]
ar=function(neuronas)
{
red4=nnet(Healthy.life.expectancy~.,train3,size=neuronas,maxit=10000,linout=TRUE,MaxNWts=10000)
predic4=predict(red4,test3,type="raw")
predic4
sum(predic4)
Error=abs(test3$Healthy.life.expectancy-predic4)/test3$Healthy.life.expectancy*100
return(mean(Error))
}

bandera=1000

for (i in 1:40)
{
  if (ar(i*20)<bandera)
  {
    mejor=i
    bandera=ar(i*20)
  }
}
mejor

redfinal=nnet(Healthy.life.expectancy~.,train3,size=26*20,maxit=10000,linout=TRUE,MaxNWts=10000)
predicmejor=predict(redfinal,test3,type="raw")
predicmejor
sum(predicmejor)
Errormejor=abs(test3$Healthy.life.expectancy-predicmejor)/test3$Healthy.life.expectancy*100
Errormejor
mean(Errormejor)
fivenum(Errormejor)
var(Errormejor)
x11()
plot(sort(Errormejor),type="b")
qqnorm(Errormejor)
hist(Errormejor,breaks=10,col="blue",freq=FALSE)


#Clusterización con K-means#

set.seed(408)
km=kmeans(recorte,4)
km$cluster
km$centers
km$size
km$iter
km$totss
km$betweenss
x11()
plot(recorte$Freedom.to.make.life.choices,recorte$Social.support,ylab="Variable dependiente",xlab="Variable independiente", col = km$cluster)
legend("topleft", levels(factor(km$cluster)), col = 1:4, lty=1)

set.seed(306)
km2=kmeans(hapiness2[,-2],3)
km2$cluster
km2$centers
km2$size
km2$iter
km2$totss
km2$betweenss
x11()
plot(hapiness$Social.support,hapiness$Healthy.life.expectancy,ylab="Variable dependiente",xlab="Variable independiente", col = km2$cluster)
legend("topleft", levels(factor(km$cluster)), col = 1:3, lty=1)


#Modelización con Random Forest#

hapiness6=hapiness
hapiness6$Country.name=NULL
partido=createDataPartition(p=0.7,y=hapiness6$Regional.indicator,list=FALSE)
traint=hapiness6[partido,]
testt=hapiness6[-partido,]
traint

random_tune=function(pmtry,pntree)
{
  hapiness6=hapiness
  hapiness6$Country.name=NULL
  partido=createDataPartition(p=0.7,y=hapiness6$Regional.indicator,list=FALSE)
  traint=hapiness6[partido,]
  testt=hapiness6[-partido,]
  
RF=randomForest(Regional.indicator~.,traint,mtry=pmtry,importance=TRUE,ntree=pntree)
randompredict=predict(RF,testt)
resultante=data.frame(testt,as.data.frame(randompredict))
resultante2=data.table(resultante)
randompredict
nrow(resultante)
resultante2[ , aciertos:= 0 ]
resultante2[randompredict==Regional.indicator,aciertos:=1]
return(sum(resultante2[,aciertos])/nrow(resultante2))
}

f=rbind(c(0,0,0))
 t0=Sys.time()
 for (i in 1:10)
   {
       for (j in 1:10)
         {
             parcial=random_tune(i+2,j*100)
             if (parcial>acc)
               {
                   mejor_try=i
                   mejor_tree=j
                   acc=parcial
                 }
             vec=c(i+2,j*100,parcial)
             f=rbind(f,vec)
             j=j+1
           }
       i=i+1
     }
t1=Sys.time()
t1-t0
dim(f)
f=data.frame(f)
names(f)=c("MTRY","MTREE","TEST")
f=data.table(f)
f[TEST==max(f[,TEST]),]
set.seed(484)
random_tune(5,1000)
random_tune(7,300)
random_tune(7,1000)


#Modelización con RPART#

hapiness7=data.table(hapiness6)
hapiness7[,clasif:=0]
hapiness7$clasif
hapiness7[,clasif:=as.numeric(Regional.indicator)]
hapiness7[,c("Regional.indicator","clasif")]
dcast(data=hapiness7,Regional.indicator~clasif)
hapiness7[,clasif:=as.factor(clasif)]


arbolin=function(semilla,split1,bucket1,depth1)
{
set.seed(semilla)
Parti_arbol=sample(1:nrow(hapiness7),round(nrow(hapiness7)*0.7,digits = 0),replace = F)
train_ar=hapiness7[Parti_arbol,]
test_ar=hapiness7[-Parti_arbol,]
set.seed(semilla)
RPART=rpart(clasif~.,data=train_ar,minsplit=split1,minbucket=bucket1,maxdepth=depth1,xval=0)
treepred=predict(RPART,test_ar,type="vector")
test_ar=cbind(test_ar,treepred)
test_ar[,valid:=0]
test_ar[clasif==treepred,valid:=1]
return (sum(test_ar[,valid])/nrow(test_ar))
}

semillas=c(120,92,11,1345,19,6,1000,14,184,22,44,32,98,10,9999,888,13,432,12,1006,51414,528,66,2,9,90,121,4004,6802,901,898,120,333,3,61,1070,82,28,18,619,2021,2018,1996)
matr=c()
for (i in 1:12)
{
  for (j in 1:6)
  {
    for (k in 1:5)
    {
      lista_semilla=c()
      for (l in 1:length(semillas))
      {
        lista_semilla[l]=arbolin(semillas[l],j+4,k+2,i+1)
      }
      lista_semilla[l+1]=i+1
      lista_semilla[l+2]=j+4
      lista_semilla[l+3]=k+2
      matr=rbind(matr,lista_semilla)
    }
  }
}

matr

nombres=c()
for (i in 1:length(semillas))
{
  nombres[i]=paste("Muestra ",i)
}
nombres[length(semillas)+1]="Maxdepth"
nombres[length(semillas)+2]="Minsplit"
nombres[length(semillas)+3]="Minbucket"
data_results=data.frame(matr)
names(data_results)=nombres
data_results=data.table(data_results)
Promedio=apply(data_results[,1:length(semillas)],1,mean)
Varianza=apply(data_results[,1:length(semillas)],1,var)
data_results=cbind(data_results,Varianza,Promedio)
data_results
data_results$Promedio
data_results$Varianza

data_results[,Prob90:=qnorm(p=0.9,mean=Promedio,sd=sqrt(Varianza))]
data_results$Prob90
data_results[,Prob30:=qnorm(p=0.3,mean=Promedio,sd=sqrt(Varianza))]
data_results[,Prob40:=qnorm(p=0.4,mean=Promedio,sd=sqrt(Varianza))]
data_results[,Prob80:=qnorm(p=0.8,mean=Promedio,sd=sqrt(Varianza))]
data_results[,Prob95:=qnorm(p=0.95,mean=Promedio,sd=sqrt(Varianza))]
data_results[1:10,c("Promedio","Varianza","Prob30","Prob40","Prob80","Prob90","Prob95")]
data_results[,Prob5:=qnorm(p=0.05,mean=Promedio,sd=sqrt(Varianza))]
data_results[80:100,c("Promedio","Varianza","Prob5","Prob95")]

max(data_results[,Promedio])
max(data_results[,Prob5])
data_results[Promedio==max(data_results[,Promedio]),c("Promedio","Varianza","Prob5","Prob95")]
data_results[Prob5==max(data_results[,Prob5]),c("Promedio","Varianza","Prob5","Prob95","Maxdepth","Minsplit","Minbucket")]
