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

#PUNTO 2: ROI
# A. Calcular el ROI por película
data <- data %>%
    mutate(ROI_US = (Total.U.S..Gross - Budget) / Budget)

# Visualizar el ROI por película
print(data %>%select(Movie, ROI_US))

# Calcular la media del ROI
mean(data$ROI_US)
summary(data$ROI_US)
## El ROI promedio de las peliculas es de aproximadamente 29,3%, lo que indica que las peliculas generan un retorno sobre la inversion positivo en promedio.

# B. Intervalo de confianza del 95% para la media del ROI de las peliculas
t.test(data$ROI_US)

# C. Es mayor al 12%
t.test(data$ROI_US, mu = 0.12, alternative = "greater")
## El ROI promedio de las películas es significativamente mayor al 12% (p-value < 0.05), lo que sugiere que, en promedio, las películas generan un retorno sobre la inversión superior al 12%.

#PUNTO 3: comparar el ROI entre comedias y no comedias
# A. calcular diferencia en ingresos
t.test(Total.U.S..Gross ~ (Genre == "Comedy"), data = data)

# B. Diferencia en el ROI entre comedias y no comedias
t.test(ROI_US ~ (Genre == "Comedy"), data = data)

#PUNTO 4: Peliculas R vs no R
# Diferencia en ingresos entre películas R y no R
t.test(Total.U.S..Gross ~ MPAA_D, data = data)

# PUNTO 5: Regresión pre-producción - Total US Gross
data <- data %>%
  mutate(Is_Comedy = as.integer(Genre == "Comedy"))

# 5a. Modelo completo con todos los factores de pre-producción
model_5a <- lm(Total.U.S..Gross ~ Budget + Is_Comedy + MPAA_D + Known.Story + Sequel,
               data = data)
summary(model_5a)

# 5b. Modelo final: eliminar variables con p > 0.10
# Is_Comedy (p=0.103), MPAA_D (p=0.688), Known.Story (p=0.230) - se eliminan
model_5b <- lm(Total.U.S..Gross ~ Budget + Sequel,
               data = data)
summary(model_5b)

# 5c.
coef(model_5b)["Sequel"]
# Manteniendo Budget fijo, una secuela genera ~$30.5M más en taquilla doméstica

# PUNTO 6: Regresión pre-opening weekend - Opening Gross
# 6a. Modelo completo: factores pre-producción + factores del opening weekend
model_6a <- lm(Opening.Gross ~ Budget + Is_Comedy + MPAA_D + Known.Story + Sequel +
                 Summer + Holiday + Christmas + Opening.Theatres,
               data = data)
summary(model_6a)

# 6b. Modelo final: solo variables significativas al 10%
# Se conservan: Budget (p=0.003), Sequel (p=0.006), Opening.Theatres (p<0.001)
model_6b <- lm(Opening.Gross ~ Budget + Sequel + Opening.Theatres,
               data = data)
summary(model_6b)

# 6c. Coeficientes 
coef(model_6b)

# 6d. IC 95% para el cambio en Opening Gross si Opening.Theatres aumenta en 100
b_th  <- coef(model_6b)["Opening.Theatres"]
se_th <- summary(model_6b)$coefficients["Opening.Theatres", "Std. Error"]
t_crit <- qt(0.975, df = df.residual(model_6b))

# Resultados con print
print(paste("Estimación puntual (+100 teatros):", round(100 * b_th)))
print(paste("IC 95%: [",
            round(100 * (b_th - t_crit * se_th)), ",",
            round(100 * (b_th + t_crit * se_th)), "]"))
