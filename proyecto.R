########################################
## Universidad del Valle de Guatemala ##
## Proyecto 1 Mineria de datos        ##
## Autores:                           ##
##    Sergio Marchena                 ##
##    Gabriel Martinez                ##
##    Michelle Bloomfield             ##
########################################

library(ggplot2)
library(caret)

data <- read.csv("C:\\Users\\Usuario\\Documents\\Septimo Semestre\\Mineria de datos\\Proyecto-Mineria-\\base proyecto - bases.csv")
data <- read.csv("base.csv")

################################----- ANALISIS EXPLORATORIO -------#########################################

##Resumen de los datos
summary(data)

##Normalidad de datos 

#Edad
qqnorm(data$EDAD)
qqline(data$EDAD)


##Tablas de frecuencia

##DEPARTAMENTO
departamento <- table(data$DEPARTAMENTO)
departamento
##TIPO DE FALTA
tipo <- table(data$FALTA)
tipo
##SEXO
sexo <- table(data$SEXO)
sexo
##ETNIA
etnia <- table(data$ÉTNIA)
etnia
##ESTADO CONYUGAL 
estCon <- table(data$EST.CONYUGAL)
estCon
##DEPARTAMENTO DE NACIMIENTO
depNac <- table(data$DEPTO..DE.NACIMIENTO)
depNac
##COND ALFABETISMO
condAlf <- table(data$COND.ALFABETISMO)
condAlf
##OCUPACION HABITUAL
ocpHab <- table(data$OCUPACION.HABITUAL)
ocpHab
##AREA GEOGRAFICA 
area <- table(data$AREA.GEOGRAFICA)
area
##ESTADO DE EBRIEDAD
estado <- table(data$ESTADO.EBRIEDAD)
estado
##ESCOLARIDAD 
esc <- table(data$ESCOLARIDAD)
esc
##A?O
year <- table(data$A?.O)
year
##MES
mes <- table(data$MES)
mes


##ESCOLARIDAD - DEPARTAMENTO
?cor
correlacion <- cor(data$DEPARTAMENTO,data$ESCOLARIDAD, "pearson", use = "complete.obs")
correlacion
library("ggpubr")
install.packages("ggpubr")
plot(data$SEXO,data$MUNICIPIO)

cor.test(data$ESTADO.EBRIEDAD,data$FALTA,method = "pearson")
View(data)
ggscatter(data, x = "AÑO", y = "FALTA", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "ESTADO", ylab = "FALTA")

quince<-nrow(data[data$DEPARTAMENTO == 1 & data$AÑO == "2015",])
seis<-nrow(data[data$DEPARTAMENTO == 1 & data$AÑO == "2016",])
siete<-nrow(data[data$DEPARTAMENTO == 1 & data$AÑO == "2017",])
anios<-c(quince,seis,siete)
barplot(anios,space = 0.5)
falG<-data[data$DEPARTAMENTO == 1,]$FALTA
hist(falG, main = "Tipo de Faltas en Guatemala", xlab = "Falta", ylab = "Casos",col = "black")
nrow(data[data$DEPARTAMENTO == 1 & data$FALTA == 5,])


faltasTot<-(data$FALTA)
faltasTotG<-nrow(data[data$DEPARTAMENTO == 1,])
mese<-data$MES
hist(mese)
cor.test(mese,faltasTot)
(data[data$MES & data$ESTADO.EBRIEDAD,])
################################---------- CLUSTERING -------------#########################################
dataCompleto <- data.frame(departamento = data$DEPARTAMENTO, mes = data$MES, year = data$AÃ.O, falta = data$FALTA, sexo = data$SEXO, edad = data$EDAD, etnia = data$Ã.TNIA, estadoConyugal = data$EST.CONYUGAL, departamentoNacimiento = data$DEPTO..DE.NACIMIENTO, condAlfabetismo = data$COND.ALFABETISMO, escolaridad = data$ESCOLARIDAD, ocupacionhabitual = data$OCUPACION.HABITUAL, estadoEbriedad = data$ESTADO.EBRIEDAD, area = data$AREA.GEOGRAFICA)

#Metodo de ward de varianza mínima para formar la gráfica de codo
wss <- (nrow(dataCompleto[,1:14])-1)*sum(apply(dataCompleto[,1:14],2,var))
for (i in 2:10) 
  wss[i] <- sum(kmeans(dataCompleto[,1:14], centers=i)$withinss)

plot(1:10, wss, type="b", xlab="Numero de clusters",  ylab="Within grupo de sumas de cuadrados")

km<-kmeans(dataCompleto[,1:14],3)
dataCompleto$grupo<-km$cluster


g1<- dataCompleto[dataCompleto$grupo==1,]
prop.table(table(g1$Species))*100
nrow(g1)
summary(g1)

g2<- dataCompleto[dataCompleto$grupo==2,]
prop.table(table(g2$Species))*100
summary(g2)

g3<- dataCompleto[dataCompleto$grupo==3,]
prop.table(table(g3$Species))*100
summary(g3)

plotcluster(dataCompleto[,1:14],km$cluster) #grafica la ubicaciÃ³n de los clusters



## INICIO DE CODIGO AÑADIDO
barplot(table(data$COND.ALFABETISMO[data$EDAD>=0 & data$EDAD<=15])/length(data$COND.ALFABETISMO[data$EDAD>=0 & data$EDAD<=15]),
        ylab="Porcentaje", xlab = "Alfabetismo",col='#001133', ylim = c(0,1))
barplot(table(data$COND.ALFABETISMO[data$EDAD>15 & data$EDAD<20])/length(data$COND.ALFABETISMO[data$EDAD>15 & data$EDAD<20]),
        ylab="Porcentaje", xlab = "Alfabetismo",col='#001133', ylim = c(0,1))
barplot(table(data$COND.ALFABETISMO[data$EDAD>=20 & data$EDAD<30])/length(data$COND.ALFABETISMO[data$EDAD>=20 & data$EDAD<30]),
        ylab="Porcentaje", xlab = "Alfabetismo",col='#001133', ylim = c(0,1))
barplot(table(data$COND.ALFABETISMO[data$EDAD>=30 & data$EDAD<40])/length(data$COND.ALFABETISMO[data$EDAD>=30 & data$EDAD<40]),
        ylab="Porcentaje", xlab = "Alfabetismo",col='#001133', ylim = c(0,1))
barplot(table(data$COND.ALFABETISMO[data$EDAD>=40 & data$EDAD<900])/length(data$COND.ALFABETISMO[data$EDAD>=40 & data$EDAD<900]),
        ylab="Porcentaje", xlab = "Alfabetismo",col='#001133', ylim = c(0,1))


barplot(table(data$FALTA[data$COND.ALFABETISMO==1])/length(data$FALTA[data$COND.ALFABETISMO==1]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.5))
barplot(table(data$FALTA[data$COND.ALFABETISMO==2])/length(data$FALTA[data$COND.ALFABETISMO==2]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.5))
barplot(table(data$FALTA[data$COND.ALFABETISMO==9])/length(data$FALTA[data$COND.ALFABETISMO==9]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,1))


barplot(table(data$FALTA[data$SEXO==1])/length(data$FALTA[data$SEXO==1]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.5))
barplot(table(data$FALTA[data$SEXO==2])/length(data$FALTA[data$SEXO==2]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,1))
barplot(table(data$SEXO)/length(data$SEXO),
        ylab="Porcentaje", xlab = "Sexo",col='#001133', ylim = c(0,1))


barplot(table(data$FALTA[data$ESTADO.EBRIEDAD==1])/length(data$FALTA[data$ESTADO.EBRIEDAD==1]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$FALTA[data$ESTADO.EBRIEDAD==2])/length(data$FALTA[data$ESTADO.EBRIEDAD==2]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.8))
barplot(table(data$ESTADO.EBRIEDAD)/length(data$ESTADO.EBRIEDAD),
        ylab="Porcentaje", xlab = "Estado de ebriedad",col='#001133', ylim = c(0,1))

barplot(table(data$FALTA[data$ESCOLARIDAD==1])/length(data$FALTA[data$ESCOLARIDAD==1]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$FALTA[data$ESCOLARIDAD==2])/length(data$FALTA[data$ESCOLARIDAD==2]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$FALTA[data$ESCOLARIDAD==3])/length(data$FALTA[data$ESCOLARIDAD==3]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$FALTA[data$ESCOLARIDAD==4])/length(data$FALTA[data$ESCOLARIDAD==4]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$FALTA[data$ESCOLARIDAD==5])/length(data$FALTA[data$ESCOLARIDAD==5]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$FALTA[data$ESCOLARIDAD==6])/length(data$FALTA[data$ESCOLARIDAD==6]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$ESCOLARIDAD)/length(data$ESCOLARIDAD),
        ylab="Porcentaje", xlab = "Grado de escolaridad",col='#001133', ylim = c(0,1))

barplot(table(data$FALTA[data$Ã.TNIA==1])/length(data$FALTA[data$Ã.TNIA==1]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$FALTA[data$Ã.TNIA==2])/length(data$FALTA[data$Ã.TNIA==2]),
        ylab="Porcentaje", xlab = "Tipo de falta",col='#001133', ylim = c(0,.6))
barplot(table(data$Ã.TNIA)/length(data$Ã.TNIA),
        ylab="Porcentaje", xlab = "Etnia",col='#001133', ylim = c(0,1))

##########################################################################################
##########################################################################################
##########################################################################################
############################## PRUEBA DE ALGORITMOS ######################################
##########################################################################################
##########################################################################################

# NAIVE BAYES
library(e1071)
library(caret)

datosTraining<-datosTraining[datosTraining$EDAD < 100,]
max(datosTraining$EDAD)

porcentaje<-0.7
datosTraining<-read.csv("base.csv")
set.seed(123)

datosTraining$X<-NULL
datosTraining$X.1<-NULL
datosTraining$X.2<-NULL
datosTraining$X.3<-NULL
datosTraining$X.4<-NULL
datosTraining$X.5<-NULL
datosTraining$X.6<-NULL
datosTraining$X.7<-NULL
datosTraining$X.8<-NULL
datosTraining$X.9<-NULL
datosTraining$CORRELATIVO<-NULL

View(datosTraining)

corte<-sample(nrow(datosTraining),nrow(datosTraining)*porcentaje)
train<-datosTraining[corte,]
test<-datosTraining[-corte,]

modelo<-naiveBayes(as.factor(FALTA)~.,data=train)

predBayes<-predict(modelo, newdata = test)
cfmBayes<-confusionMatrix(predBayes,as.factor(test$FALTA))
cfmBayes
#Accuracy : 0.4426 :'(


# RANDOM FOREST
library(randomForest)
library(caret)


