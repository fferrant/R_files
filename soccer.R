
# Limpio la memoria

rm(list=ls())
gc()

# Cargo las librerías que voy a utilizar

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
library(naivebayes)

# Seteo el directorio en el cual quiero trabajar

setwd("~/RETAIL")

# Cargo los datasets que voy a utilizar

soccer=read.csv2("soccer.csv",header=TRUE,sep=";")
soccer=data.frame(soccer)
soccer=data.table(soccer)
footbal=read.csv2("soccer2.csv",header=TRUE,sep=";")
footbal=data.frame(footbal)
footbal=data.table(footbal)
footbal[is.na(footbal)]=0
soccer[is.na(soccer)]=0

# Obteniendo algunos parámetros básicos de nuestros datasets

summary(soccer)
dim(soccer)
names(soccer)
head(soccer)
summary(soccer$state)
head(soccer$sport)
unique(soccer$year)
sum(soccer$gp)

summary(footbal)
dim(footbal)
head(footbal)
names(footbal)
unique(footbal$sport)
ftable(footbal$state)
sub=group_by(footbal,sport)
sub2=group_by(footbal,state,sum(X2018.2019))
sub2$`sum(X2018.2019)`

#Realizando algunos gráficos básicos
soccer2=soccer[gp!=0]
x11()
plot(soccer2$gp~soccer2$year,col=rainbow(50),label=soccer$state)
x11()
boxplot(soccer2$gp~soccer2$year,col='blue',main="Cantidad de jugadoras por año",xlab="Año",ylab="Cantidad")
hist(soccer2$gp)
x11()
barplot(sort(soccer2$gp))
fivenum(soccer2$gp)
stem(soccer2$gp)
soccer$es_cero=NULL
soccer[,es_cero:='NO']
soccer[gp==0,es_cero:='SI']
summary(soccer$es_cero)
ftable(soccer$es_cero)

#Elaboro una red neuronal para minimizar el error en la participación de mujeres adolescentes

soccer$Row=NULL
soccer$Column=NULL
names(soccer3)
soccer3=soccer
soccer3$gp=NULL
soccer3$gr=NULL
set.seed(80)
particion=createDataPartition(y=soccer3$es_cero,p=0.1,list=FALSE)
train=soccer3[particion,]
test=soccer3[-particion,]
red=nnet(es_cero~.,train,size=5,maxit=100000,linout=FALSE,MaxNWts=100000)
summary(train)
names(footbal)
footbal[,es_cero:='NO']
footbal[X2018.2019==0,es_cero:='SI']
ftable(footbal$es_cero)
footbal2=footbal
footbal2$X2018.2019=NULL
set.seed(80)
particion=createDataPartition(y=footbal2$X2017.2018,p=0.5,list=FALSE)
train=footbal2[particion,]
test=footbal2[-particion,]

red_de_redes=function(neuronas,semilla)
{
set.seed(semilla)
particion=createDataPartition(y=footbal2$X2017.2018,p=0.5,list=FALSE)
train=footbal2[particion,]
test=footbal2[-particion,]
red=nnet(X2017.2018~.,train,size=neuronas,maxit=100000,linout=TRUE,MaxNWts=100000)
predecir=predict(red,test)
error=abs(predecir-test$X2017.2018)/test$X2017.2018*100
return(mean(error))

}

minimo=100000
neuro=0

for (i in 20:60)
{
  desafio=red_de_redes(i,50)
  if (desafio<minimo)
  {
    minimo=desafio
    neuro=i
  }
  
}

minimo
neuro

#Elaboro un arbol que busque minimizar el error en la cantidad de mujeres que practican cada deporte

names(soccer)
soccer$es_cero=NULL
soccer_sub=soccer[1:50,]
parti2=createDataPartition(y=soccer$gp,p=0.7,list=FALSE)
train2=soccer[parti2,]
test=soccer[-parti2,]
modelo=rpart(gp~.,data=train2,minsplit=10,minbucket=5,maxdepth=12,xval=0)
summary(modelo)
modelo$variable.importance
modelo_pred=predict(modelo,test,type="vector")
dim(modelo_pred)
error3
sum(abs(modelo_pred-test$gp))
sum(test$gp)
length(test$gp)
sum(abs(modelo_pred-test$gp))/length(test$gp)

arbolitus=function(split,bucket,depth,semilla)
{
  minus=10000
  vec=c()
  set.seed(semilla)
  partif=createDataPartition(y=soccer$gp,p=0.7,list=FALSE)
  trainf=soccer[partif,]
  testf=soccer[-partif,]
  for (i in 1:split)
  {
    for (j in 1:bucket)
    {
      for (k in 1:depth)
      {
        mozart=rpart(gp~.,data=trainf,minsplit=i,minbucket=j,maxdepth=k,xval=0)
        pr=predict(mozart,testf)
        mistake=sum(abs(pr-testf$gp))/sum(testf$gp)
        if (mistake<minus)
        {
          minus=mistake
          vec=c(i,j,k,mistake)
        }
      }
    }
  }
  
  return (vec)      
}  
t0=Sys.time()  
resul=arbolitus(40,20,30,6873)
t1=Sys.time()
t1-t0
resul

#Elaboro un análisis de randomforest que busque minimizar el error en la cantidad de mujeres que practican cada deporte


set.seed(1124)
parti4=createDataPartition(y=soccer$gp,p=0.7,list=FALSE)
parti5=sample(1:length(soccer$gp),round(0.7*length(soccer$gp),digits=0),replace=F)
soccer10=soccer
soccer10$sport=NULL
train4=soccer10[parti4,]
train5=soccer10[parti5,]
test4=soccer10[-parti4,]
test5=soccer10[-parti5,]
modelo4=randomForest(gp~.,data=train4,mtry=5,ntree=100,importance=FALSE)
modelo5=randomForest(gp~.,data=train5,mtry=5,ntree=100,importance=FALSE)
pr4=predict(modelo4,test4)
pr5=predict(modelo5,test5)
head(pr4)
head(pr5)
error4=sum(abs(test4$gp-pr4))/sum(test4$gp)
error5=sum(abs(test5$gp-pr5))/sum(test5$gp)
error4
error5
head(test4$gp)
head(test5$gp)
matt=matrix()

bosquecito=function(try,tree)
{
  matt=matrix(c(0,0,0,0),nrow=1,ncol=4)
  for (i in 2:try)
  {
    for (j in 1:tree)
    {
      modelo_4=randomForest(gp~.,data=train4,mtry=i,ntree=j*20)
      modelo_5=randomForest(gp~.,data=train5,mtry=i,ntree=j*20)
      pr_4=predict(modelo_4,test4)
      pr_5=predict(modelo_5,test5)
      er_4=sum(abs(pr_4-test4$gp))/sum(test4$gp)
      er_5=sum(abs(pr_5-test5$gp))/sum(test5$gp)
      vc=c(i,j*20,er_4,er_5)
      print(vc)
      matt=rbind(matt,vc)
    }
  }
return (matt)
}

past=bosquecito(6,10)
past=data.frame(past)
names(past)=c("mtry","ntrees","p0","p1")
past=data.table(past)
past=past[2:nrow(past),]
past[p0==min(past$p0)]
past[p1==min(past$p1)]


#Analizo aquellas filas que tienen cero participación femenina a partir de Naive-Bayes

names(soccer)
soccer4=soccer
soccer4$Row=NULL
soccer4$Column=NULL
soccer4$gp=NULL
soccer4$gr=NULL
set.seed(101)
entrena1=sample_frac(soccer4,0.7)
prueba1=setdiff(soccer4,entrena1)
dividir=createDataPartition(y=soccer4$es_cero,p=0.7,list=FALSE)
entrena2=soccer4[dividir,]
prueba2=soccer4[-dividir,]
ftable(soccer4$es_cero)
ftable(entrena1$es_cero)
ftable(entrena2$es_cero)

bayes1=naive_bayes(es_cero~.,data=entrena1)
pred1=predict(bayes1,prueba1)
head(pred1,50)
result1=confusionMatrix(pred1,as.factor(prueba1$es_cero))
summary(result1)


bayes2=naive_bayes(es_cero~.,data=entrena2)
pred2=predict(bayes2,prueba2)
head(pred2,50)
result2=confusionMatrix(pred2,as.factor(prueba2$es_cero))
summary(result2)
result2

sem=c(104,59,32,42,184,27,1003,24,53,65,13,35,37,12,24,98,99)
length(sem)

suerte=function(semilla)
{
  mat=rbind(c(0,0,0))
  for (i in 1:length(semilla))
  {
    set.seed(semilla[i])
    entrena1=sample_frac(soccer4,0.7)
    prueba1=setdiff(soccer4,entrena1)
    dividir=createDataPartition(y=soccer4$es_cero,p=0.7,list=FALSE)
    entrena2=soccer4[dividir,]
    prueba2=soccer4[-dividir,]
    
    bayes1=naive_bayes(es_cero~.,data=entrena1)
    pred1=predict(bayes1,prueba1)
    result1=confusionMatrix(pred1,as.factor(prueba1$es_cero))
    r1=as.vector(result1$overall[1])
    
    bayes2=naive_bayes(es_cero~.,data=entrena2)
    pred2=predict(bayes2,prueba2)
    result2=confusionMatrix(pred2,as.factor(prueba2$es_cero))
    r2=as.vector(result2$overall[1])
    
    vector=c(semilla[i],r1,r2)
    mat=rbind(mat,vector)
  }
  return(mat)
}

lama=suerte(sem)
print(lama)
lama=data.frame(lama)
names(lama)=c("Semilla","Predic_1","Predic_2")
lama=lama[2:nrow(lama),]
print(lama)

#Realizamos análisis de Naive-Bayes para detectar el deporte en cuestión

soccer5=soccer
soccer5$Row=NULL
soccer5$Column=NULL
set.seed(41455)
div=createDataPartition(y=soccer5$sport,p=0.7,list=FALSE)
entrena3=soccer5[div,]
testea3=soccer5[-div,]
entrena4=sample_frac(soccer5,0.7)
testea4=setdiff(soccer5,entrena4)

bayes3=naive_bayes(sport~.,data=entrena3)
pred3=predict(bayes3,testea3)
head(pred3,50)
conf3=confusionMatrix(pred3,as.factor(testea3$sport))
conf3$overall
conf3$overall[1]

bayes4=naive_bayes(sport~.,data=entrena4)
pred4=predict(bayes4,testea4)
head(pred4,50)
conf4=confusionMatrix(pred4,as.factor(testea4$sport))
conf4$overall
conf4$overall[1]

#Estimación de gp utilizando regresión múltiple

names(soccer)
soccer10=soccer[,c("gp","bp","gr","br")]
train10=sample_frac(soccer10,0.7)
test10=setdiff(soccer10,train10)
multiple=lm(gp~.,data=train10) #modelo CON intercepción
multiple
summary(multiple)
yasam=predict(multiple,test10)
head(yasam,50)
acc=1-sum(abs(yasam-test10$gp))/sum(test10$gp)
acc
residuos=yasam-test10$gp
head(residuos,50)
x11()
plot(residuos)
boxplot(residuos)
qqnorm(residuos)
qqline(residuos)

names(soccer10)
multiple2=lm(gp~bp+gr+br-1,data=train10) #modelo SIN intercepción
multiple2
summary(multiple2)
yasam2=predict(multiple2,test10)
head(yasam2,50)
acc2=1-sum(abs(yasam2-test10$gp))/sum(test10$gp)
acc2
residuos2=yasam2-test10$gp
head(residuos2,50)
x11()
plot(residuos2)
boxplot(residuos2)
qqnorm(residuos2)
qqline(residuos2)

shapiro.test(residuos)
shapiro.test(residuos2)
X11()
hist(residuos,density = TRUE,col='green',main="Histograma de residuos")
hist(residuos2,density = TRUE,col='blue',main="Histograma de residuos V2")
stem(residuos)
stem(residuos2)


#Análisis de regresión logística para determinar donde es_cero=Verdadero

soccer11=soccer
soccer11$Row=NULL
soccer11$Column=NULL
soccer11$es_cero=factor(soccer11$es_cero)
summary(soccer11$es_cero)
set.seed(303)
grecia=createDataPartition(y=soccer11$es_cero,p=0.7,list=FALSE)
train20=soccer11[grecia,]
test20=soccer11[-grecia,]
names(train20)
modeloa=glm(as.factor(es_cero)~bp+br,data=train20,family="binomial") #modelo varaibles numéricas con intersección
modeloa
summary(modeloa)
coefficients(modeloa)
sub_predict=predict(modeloa,test20)
head(sub_predict,30)
proba=exp(sub_predict)/(1+exp(sub_predict))
head(proba,100)
ftable(test20$es_cero)
test20=cbind(test20,proba)
names(test20)
test20=data.table(test20)
test20[,predic:="NO"] 
test20[proba>0.272,predic:="SI"]
summary(test20$predic)
summary(test20$es_cero)
test20[,aciertos:=0]
test20[es_cero==predic,aciertos:=1]
ftable(test20$aciertos)
accuracy=sum(test20[,aciertos])/nrow(test20)
accuracy
ftable(test20$es_cero~test20$predic)
head(test20[,c("es_cero","predic")],50)
nrow(test20)   
sum(test20[,aciertos])



soccer12=soccer
soccer12$Row=NULL
soccer12$Column=NULL
soccer12$es_cero=factor(soccer12$es_cero)
summary(soccer12$es_cero)
set.seed(999)
belgica=createDataPartition(y=soccer12$es_cero,p=0.7,list=FALSE)
train30=soccer12[belgica,]
test30=soccer12[-belgica,]
names(train30)
modelob=glm(as.factor(es_cero)~bp+br-1,data=train30,family="binomial") #modelo varaibles numéricas sin intersección
modelob
summary(modelob)
coefficients(modelob)
sub_predictb=predict(modelob,test30)
head(sub_predictb,30)
probab=exp(sub_predictb)/(1+exp(sub_predictb))
head(probab,100)
ftable(test30$es_cero)
test30=cbind(test30,probab)
names(test30)
test30=data.table(test30)
test30[,predic:="NO"] 
test30[probab>0.272,predic:="SI"]
summary(test30$predic)
summary(test30$es_cero)
test30[,aciertos:=0]
test30[es_cero==predic,aciertos:=1]
ftable(test30$aciertos)
accuracyb=sum(test30[,aciertos])/nrow(test30)
accuracyb
ftable(test30$es_cero~test30$predic)
head(test30[,c("es_cero","predic")],50)
nrow(test30)   
sum(test30[,aciertos])

