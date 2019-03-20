########################################
## Universidad del Valle de Guatemala ##
## Proyecto 1 Mineria de datos        ##
## Authores:                          ##
##    Sergio Marchena                 ##
##    Gabriel Martinez                ##
##    Michelle Bloomfield             ##
########################################

library(ggplot2) 

data <- read.csv("C:\\Users\\Usuario\\Documents\\Septimo Semestre\\Mineria de datos\\Proyecto-Mineria-\\base proyecto - bases.csv")


################################----- ANALISIS EXPLORATORIO -------#########################################

##Resumen de los datos
summary(data)

##Normalidad de datos 
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
etnia <- table(data$Ã.TNIA)
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
esc <- ta
ble(data$ESCOLARIDAD)
esc
################################---------- CLUSTERING -------------#########################################