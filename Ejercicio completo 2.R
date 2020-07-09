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


#Carga de datos#

setwd("~/RETAIL")
tasmania=read.csv2("tasmania.csv")
tasmania=data.frame(tasmania)
tasmania=data.table(tasmania)
summary(tasmania)
head(tasmania)
fivenum(tasmania$age_on_arrival_in_years,na.rm=TRUE)
tasmania$age_on_arrival_in_years
ftable(tasmania$religion,tasmania$do_you_go_to_school)
ftable(tasmania$region,tasmania$district)
ftable(tasmania$religion)
ftable(tasmania$region)
stem(tasmania$age_on_arrival_in_years)
mean(tasmania$age_on_arrival_in_years,na.rm=TRUE)
var(tasmania$age_on_arrival_in_years,na.rm=TRUE)
mode(tasmania$age_on_arrival_in_years)
median(tasmania$age_on_arrival_in_years,na.rm=TRUE)


#Realizando algunos gráficos generales#

x11()
boxplot(tasmania$age_on_arrival_in_years,main="Boxplot",col="brown")
hist(tasmania$age_on_arrival_in_years,main="Boxplot",col="brown",probability = TRUE,xlab="Years range",ylab="Frequency")
plot(tasmania)

#Agrupamiento de puntos por kohonen
tasmania3=tasmania
tasmania3$id=NULL
tasmania3$date_of_arrival_at_safe_house=NULL
tasmania3$do_you_go_to_school=NULL
tasmania3$region=NULL
tasmania3$district=NULL
tasmania3$religion=NULL
tasmania3$type_of_case=NULL
tasmania3$who_brought_her_to_the_safe_house=NULL
tasmania3$suggestion_from_social_welfare_officer=NULL
tasmania3=as.matrix(tasmania3)
tasmania3
koho=som(tasmania3,grid=somgrid(4,5,"hexagonal"))
summary(koho)
names(koho)
unique(koho$unit.classif)
sort(unique(koho$unit.classif))
x11()
plot(koho,type="codes")
plot(koho,type="count")
plot(koho,type="mapping")
plot(koho,type="change")
koho$codes
koho$maxNA.fraction
lista=koho$dist.fcts
lista


#Clustering utilizando K-means

tasmania4=tasmania
tasmania4$id=NULL
tasmania4$date_of_arrival_at_safe_house=NULL
tasmania4$do_you_go_to_school=NULL
tasmania4$region=NULL
tasmania4$district=NULL
tasmania4$religion=NULL
tasmania4$type_of_case=NULL
tasmania4$who_brought_her_to_the_safe_house=NULL
tasmania4$suggestion_from_social_welfare_officer=NULL

clusters=kmeans(tasmania4[1:10,],4)

tasmania4=na.roughfix(tasmania4)
clusters=kmeans(tasmania4,4)
summary(clusters)
listita=clusters$cluster
listita
matriz_cluster=c()
for (i in 1:4)
{
  Clustering=kmeans(tasmania4,i+1)
  matriz_cluster=cbind(matriz_cluster,Clustering$cluster)
}
matriz_cluster=data.frame(matriz_cluster)
vector_nom=c()
for (j in 1:ncol(matriz_cluster))
{
  vector_nom[j]=paste0("Clustering_",j+1)
}
vector_nom

names(matriz_cluster)=vector_nom
tasmania=cbind(tasmania,matriz_cluster)
tasmania=data.table(tasmania)
tasmania
tasmania[,Cluster_total:=paste0(Clustering_2,Clustering_3,Clustering_4,Clustering_5)]
tasmania$Cluster_total

km2=kmeans(tasmania4,3)
km2$cluster
km2$centers
km2$size
km2$iter
km2$totss
km2$betweenss


#Clasificación por SVM (Support Vector Machines)#

tasmania$id=NULL
tasmania$date_of_arrival_at_safe_house=NULL
tasmania=as.factor(tasmania)
tasmania$region
modelo_svm=svm(formula=tasmania$age_on_arrival_in_years~tasmania$health_status_healthy+tasmania$health_status_sti+tasmania$health_status_uti+tasmania$health_status_pregnant+tasmania$health_status_other_health_issue+tasmania$immediate_needs_on_arrival_clothes+tasmania$immediate_needs_on_arrival_underwear+tasmania$immediate_needs_on_arrival_shoes+tasmania$immediate_needs_on_arrival_counselling+tasmania$immediate_needs_on_arrival_other,data=tasmania,kernel="linear",cost=10,scale=FALSE)
summary(modelo_svm)
modelo_svm$levels
modelo_svm$index
x11()
plot(modelo_svm,tasmania)

iris$Species
supo=iris
supo$Species=as.factor(supo$Species)
head(supo)
supo_svm= tune("svm",supo$Species~supo$Sepal.Length+supo$Sepal.Width+supo$Petal.Length+supo$Petal.Width,
                data = supo, kernel = 'linear',
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20, 50, 100, 150, 200)))
summary(supo_svm)


supo_svm2= svm(supo$Species~supo$Sepal.Length+supo$Sepal.Width+supo$Petal.Length+supo$Petal.Width,
               data = supo, kernel = 'linear',cost=10)
x11()
particione=createDataPartition(y=supo$Species,p=0.7,list=FALSE)
traine=supo[particione,]
teste=supo[-particione,]
svm1=svm(Species~.,data=traine,kernel="linear")
aux1=predict(svm1,teste)
teste1=cbind(teste,aux1)
teste1=data.table(teste1)
teste1[,eval:=0]
teste1[Species==aux1,eval:=1]
sum(teste1[,eval])/nrow(teste1)

x11()
plot(supo$Sepal.Length~supo$Sepal.Width,col=as.factor(supo$Species))

svm2=svm(Species~Sepal.Length+Sepal.Width,data=traine,kernel="linear")
aux2=predict(svm2,teste)
teste2=cbind(teste,aux2)
teste2=data.table(teste2)
teste2[,eval:=0]
teste2[Species==aux2,eval:=1]
sum(teste2[,eval])/nrow(teste2)

x11()
plot(supo$Petal.Length~supo$Petal.Width,col=as.factor(supo$Species),pch=7,cex=0.5)

svm3=svm(Species~Sepal.Length+Sepal.Width,data=traine,kernel="sigmoid")
aux3=predict(svm3,teste)
teste3=cbind(teste,aux3)
teste3=data.table(teste3)
teste3[,eval:=0]
teste3[Species==aux3,eval:=1]
sum(teste3[,eval])/nrow(teste3)
confSVM=confusionMatrix(aux3,teste3$Species)
confSVM
ftable(teste3$Species~teste3$aux3)



svm_cv6 <- tune("svm", Species ~., data = supo, kernel = 'radial',
               ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 20),
                             gamma = c(0.5, 1, 2, 3, 4, 5, 10)))
summary(svm_cv6)
x11()
ggplot(data = svm_cv6$performances, aes(x = cost, y = error, color = as.factor(gamma)))+
  geom_line() +
  geom_point() +
  labs(title = "Error de clasificación vs hiperparámetros C y gamma", color = "gamma") +
  theme_bw() +
  theme(legend.position = "bottom")

aux6=predict(svm_cv6$best.model,teste)
teste6=cbind(teste,aux6)
teste6=data.table(teste6)
teste6[,eval:=0]
teste6[Species==aux6,eval:=1]
sum(teste6[,eval])/nrow(teste6)
confSVM2=confusionMatrix(aux6,teste6$Species)
confSVM2
ftable(teste6$Species~teste6$aux6)

x11()
plot3d(supo$Sepal.Length,supo$Sepal.Width,supo$Petal.Length,col=rainbow(3),type="s")

fix(supo)
