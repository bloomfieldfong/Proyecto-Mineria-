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
etnia <- table(data$Ã‰TNIA)
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


##¿Cual es el porcentaje de menores de edad que cometen faltas? ¿Que tipo de faltas cometen?
menores <- subset(data, data$EDAD<18, select=c("EDAD", "FALTA"))
menores1 <- table(menores)
menores1

length(menores1)

##¿Cuántos menores de edad cometen faltas por estar alcoholizados?
Alchol <- subset(data, data$ESTADO.EBRIEDAD<2, select=c("EDAD", "ESTADO.EBRIEDAD"))
menoresAlcohol <-subset(Alcohol, Alcohol$EDAD<18, select=c("EDAD", "ESTADO.EBRIEDAD"))

nrow(menoresAlcohol)

##ESCOLARIDAD - DEPARTAMENTO
?cor
correlacion <- cor(data$DEPARTAMENTO,data$ESCOLARIDAD, "pearson", use = "complete.obs")
correlacion
library("ggpubr")
install.packages("ggpubr")
plot(data$DEPARTAMENTO,data$ESCOLARIDAD)

cor.test(data$DEPARTAMENTO,data$ESCOLARIDAD,method = "spearman")

ggscatter(data, x = "DEPARTAMENTO", y = "ESCOLARIDAD", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Departamento", ylab = "Grado de Escolaridad")

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








