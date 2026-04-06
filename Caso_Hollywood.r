#Instalación de las librerías necesarias
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)

# Cargar los datos
setwd('/Users/maru/Desktop/Javeriana/4/Analitica de los negocios/Caso Hollywood')
data <- read_xls('Datos_Hollywood.xls', sheet = 2)

#Visualización nombres de las columnas
colnames(data)
#arreglar el nombre de las columnas
names(data) <- make.names(names(data))
# Verificar la estructura de los datos
str(data)
#Visualización nombres finales de las columnas
colnames(data)

#PUNTO 1:
#Resumen estadístico descriptivo
summary(data$Opening.Gross)
summary(data$Total.U.S..Gross)
summary(data$Total.Non.U.S..Gross)
summary(data$Opening.Theatres)

#Calculo del número de películas de comedia
sum(data$Genre == "Comedy")

# Calculo del número de películas R
sum(data$MPAA_D == 1)

