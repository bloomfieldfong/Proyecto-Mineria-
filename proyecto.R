########################################
## Universidad del Valle de Guatemala ##
## Proyecto 1 Mineria de datos        ##
## Autores:                          ##
##    Sergio Marchena                 ##
##    Gabriel Martinez                ##
##    Michelle Bloomfield             ##
########################################

library(ggplot2) 

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
