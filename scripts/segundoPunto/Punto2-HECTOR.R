library(tidyverse)
library(dplyr)
library(ggplot2)
library(modeest)


rm(list = ls())
setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 1 ")

#Trayendo la base de datos tras el scrape 
df <- read.csv("datos.csv")
df <- df[, -1]
head(df)
summary(df$age)
#Manteniendo los mayores de 18 
df <- subset(df, age >= 18 & p6240 ==1) 
summary(df$age)
#Cuál es la mejor manera de manejar los datos 
salarioNA <- sum(is.na(df$y_ingLab_m_ha))
summary(df$y_ingLab_m_ha) #No hay personas con esta variable de w por horas en 0 
cat("Número de personas sin salario:", salarioNA) #5124 NAs 
atipico <- which.max(df$y_ingLab_m_ha)
df$y_ingLab_m_ha[atipico] <- NA

salario_horas <- df$y_ingLab_m_ha
salario_horas_NA <- na.omit(salario_horas) 

desviacion_estandar_S <- sd(salario_horas_NA)
moda <- mfv(salario_horas_NA)
media <- mean(salario_horas_NA)

std_nueva <- sd(salario_horas)

prueba_ocu <- df$ocu - df$p6240
summary(prueba_ocu) #Todos los que tienen p6240 son ocupados, no hay cosas raras. En cambio, hay personas que son ocupadas
                    #que declaran utilizar su tiempo en cosas diferentes a trabajar... 

salario_horas <- df$y_ingLab_m_ha

hist(salario_horas, 
     main = "Histograma Salario por horas",  # Título del gráfico
     xlab = "Valores",                       # Etiqueta del eje x
     ylab = "Frecuencia",                    # Etiqueta del eje y
     col = "lightblue",                      # Color de las barras del histograma
     border = "black")                       # Color del borde de las barras
     #breaks = 50)                            # Número de intervalos (barras)

densidad_kernel <- density(salario_horas)
plot(densidad_kernel)

boxplot(salario_horas) #Hacer uno por rangos de edad o algo similar 
hist(df$age) #El max son 91 años 

rangos_edad <- NA
rangos_edad <- ifelse(df$age >=18 & df$age <=30, 1, NA)
rangos_edad <- ifelse(df$age > 30 & df$age <=50, 2, rangos_edad)
rangos_edad <- ifelse(df$age > 50 & df$age <=70, 3, rangos_edad)
rangos_edad <- ifelse(df$age > 70 & df$age <=91, 4, rangos_edad)
label_edad <- c("18-30", "31-50", "51-70", "71-91")
rangos_edad <- factor(rangos_edad, levels = 1:4, labels = label_edad)

par(cex.axis = 0.8) 
boxplot(salario_horas ~ rangos_edad,
        main = "Salario según rangos de edad", 
        xlab = "Rangos de edad",
        ylab = "Salario por horas",
        col = "lightblue",
        border = "black",
        yaxt = "n")

label_y <- c("$5k", "$10k", "$15k", "$20k", "$25k", "$30k")
axis(2, at = seq(5000, 30000, by = 5000), labels = label_y, las = 1)

salario_horas_ordenado <- sort(salario_horas)
tail(salario_horas_ordenado) #Sólo hay un valor que se sale mucho de la distribución y es 350583.3 

#Tal vez lo mejor sería quitar este valor atípico para visualizar mejor los datos y estimar con mayor precisión
#salario_horas <- head(salario_horas_ordenado, -1)
atipico <- which.max(salario_horas)
salario_horas[atipico] <- NA

#Imputación de NAs con la moda 
df$y_ingLab_m_ha <- ifelse(is.na(df$y_ingLab_m_ha), moda, df$y_ingLab_m_ha)
#conteo de cuantas obs estan por encima!!!!!

#Seccion 
#1. Decisiones que hemos tomado (variable de w, ocupados, poner como NA el valor extremo)
#2. Cuantos obs hay. Valores raros (el grueso de la datos se concentra en un rango y eso hace que haya valores atipicos) ¿Cuantos? Graficos. Resumen por cuartiles  
#3. Estadisticas descriptivas antes de la imputacion
#4. NAs por media por el tema de la varianza (discutir por que no por la moda)
#5. Como se ve la base completa. Estadisticas descriptivas despues de la imputacion


#Seleccionar variables. 

primer_cuartil <- quantile(salario_horas, 0.25)
segundo_cuartil <- quantile(salario_horas, 0.50)
tercer_cuartil <- quantile(salario_horas, 0.75)
cuarto_cuartil <- quantile(salario_horas, 0.999)


