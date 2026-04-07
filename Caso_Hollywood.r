#Instalación de las librerías necesarias
library(dplyr)
library(ggplot2)
library(readr)
library(readxl)

# Cargar los datos
setwd('/Users/maru/Desktop/Javeriana/4/Analitica de los negocios/Caso Hollywood/Hollywood_case')
data <- read_xls('Datos__Hollywood.xls', sheet = 2)

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

# PUNTO 7: Total US Gross vs Opening Gross – "regla del 25%"
# 7a. Regresión simple
model_7a <- lm(Total.U.S..Gross ~ Opening.Gross, data = data)
summary(model_7a)

# 7b. Mostrar slopes
print(paste("Slope estimado:", round(coef(model_7a)["Opening.Gross"], 4)))
print("Slope que implicaría la regla del 25%: 4.0")

# 7c. Prueba de hipótesis: H0: slope = 4
b7   <- coef(model_7a)["Opening.Gross"]
se7  <- summary(model_7a)$coefficients["Opening.Gross", "Std. Error"]
t7c  <- (b7 - 4.0) / se7
p7c  <- 2 * pt(abs(t7c), df = df.residual(model_7a), lower.tail = FALSE)

print(paste("t =", round(t7c, 4), "| p-valor =", round(p7c, 6)))
#Se rechaza H0: el slope es significativamente distinto de 4.0

# 7e. Modelo sin intercepto (forzado al origen)
model_7e <- lm(Total.U.S..Gross ~ Opening.Gross - 1, data = data)
summary(model_7e)

# 7f. Prueba con modelo sin intercepto: H0: slope = 4.0
b7e  <- coef(model_7e)["Opening.Gross"]
se7e <- summary(model_7e)$coefficients["Opening.Gross", "Std. Error"]
t7f  <- (b7e - 4.0) / se7e
p7f  <- 2 * pt(abs(t7f), df = df.residual(model_7e), lower.tail = FALSE)

print(paste("Slope sin intercepto:", round(b7e, 4)))
print(paste("t =", round(t7f, 4), "| p-valor =", round(p7f, 8)))

# Conclusión: Se sigue rechazando H0: el opening representa ~30%, no el 25%

# 7g. R² del modelo simple
r2 <- summary(model_7a)$r.squared
print(paste("R² =", round(r2, 4)))
print(paste("El opening weekend explica el", round(r2 * 100, 2),
            "% de la variación en Total US Gross"))

# Gráfico: Total US Gross vs Opening Gross con líneas de regresión
ggplot(data, aes(x = Opening.Gross / 1e6, y = Total.U.S..Gross / 1e6)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred",
              linewidth = 1, aes(linetype = "Con intercepto")) +
  geom_abline(slope = 4, intercept = 0,
              color = "darkorange", linetype = "dashed", linewidth = 1) +
  geom_abline(slope = coef(model_7e)["Opening.Gross"], intercept = 0,
              color = "darkgreen", linetype = "dotted", linewidth = 1) +
  labs(
    title = "Total US Gross vs Opening Weekend Gross",
    x = "Opening Gross (millones USD)",
    y = "Total US Gross (millones USD)",
    caption = paste0(
      "Rojo (slope=", round(b7, 2), ")",
      "  |  Naranja (slope=4.0)",
      "  |  Verde (slope=", round(b7e, 2), ")"
    )
  ) +
  theme_minimal()

#PUNTO 8: Opinion de los criticos
#A. Modelo 4: Regresión para predecir la opinión de los críticos
model4 <- lm(Total.U.S..Gross ~ Opening.Gross + Critics..Opinion + Budget + Sequel + MPAA_D, data = data)
summary(model4)

#B. modelo sin variables no significativas
model4_final <- lm(Total.U.S..Gross ~ Opening.Gross + Critics..Opinion + Budget + MPAA_D, data = data)
summary(model4_final)

#C. predicción con el modelo final

predict(model4_final, newdata = data.frame(
  Opening.Gross = 10245000,   
  Critics..Opinion = 73,      
  Budget = 90000000,          
  MPAA_D = 1
), interval = "prediction")



coef(model4_final)["Critics..Opinion"]
#Que impacto tiene un +10 puntos en la opinión de los críticos
coef(model4)["Critics..Opinion"] * 10

#PUNTO 9: Iteracción comedia y opinión de los críticos
#Modelo 5: Regresión con interacción entre comedia y opinión de los críticos
model5 <- lm(Total.U.S..Gross ~ Opening.Gross + Critics..Opinion * (Genre == "Comedy") + Critics..Opinion:(Genre == "Comedy"), data = data)
summary(model5)
