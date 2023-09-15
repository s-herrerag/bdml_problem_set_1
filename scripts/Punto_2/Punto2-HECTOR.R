library(tidyverse)
library(dplyr)
library(ggplot2)
library(modeest)


rm(list = ls())
setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 1 ")

#Trayendo la base de datos tras el scrape 
df <- read.csv("datos.csv")
df <- df[, -1]
summary(df$age)
#Manteniendo los mayores de 18 
df <- subset(df, age >= 18 & p6240 ==1) #Muestra de 14.091 obs 
summary(df$age)
#Cuál es la mejor manera de manejar los datos 
salarioNA <- sum(is.na(df$y_ingLab_m_ha))
summary(df$y_ingLab_m_ha) #No hay personas con esta variable de w por horas en 0 
cat("Número de personas sin salario:", salarioNA) #5124 NAs 
atipico <- which.max(df$y_ingLab_m_ha)
df$y_ingLab_m_ha[atipico] <- NA #La persona con el valor extremo atipico tiene 42 años y es mujer

#Eliminaremos los valores atípicos de la distribución. Nos quedamos con el percentil 98 de la muestra
#Vamos a restringuir la base únicamente para las personas que tienen información del salario
primer_cuartil  <-  quantile(df$y_ingLab_m_ha, 0.25, na.rm = TRUE) 
segundo_cuartil <-  quantile(df$y_ingLab_m_ha, 0.50, na.rm = TRUE) 
tercer_cuartil  <-  quantile(df$y_ingLab_m_ha, 0.75, na.rm = TRUE) 
p95 <- quantile(df$y_ingLab_m_ha, 0.95, na.rm = TRUE) 
p98 <- quantile(df$y_ingLab_m_ha, 0.98, na.rm = TRUE) 
p99 <- quantile(df$y_ingLab_m_ha, 0.99, na.rm = TRUE) 

df <- df %>%
  filter(df$y_ingLab_m_ha <p99) #por encima o igual a de esto son 91 obs.Total obs = 8875

#df <- df %>%
  #filter(!is.na(df$y_ingLab_m_ha)) #son 8966 con las que me quedo

std_nueva <- sd(df$y_ingLab_m_ha) #Nueva varianza 8111 
write_csv(df, file = "BaseFinal.csv") 

salario_horas <- df$y_ingLab_m_ha
salario_horas_noNA <- na.omit(salario_horas) 

desviacion_estandar_S <- sd(salario_horas_noNA)
moda <- mfv(salario_horas_noNA)
media <- mean(salario_horas_noNA)

prueba_ocu <- df$ocu - df$p6240
summary(prueba_ocu) #Todos los que tienen p6240 son ocupados, no hay cosas raras. En cambio, hay personas que son ocupadas
                    #que declaran utilizar su tiempo en cosas diferentes a trabajar... 

salario_horas <- df$y_ingLab_m_ha

df <- df %>% 
  mutate(logw = log(y_ingLab_m_ha))

nueva <- df %>% 
  filter(salario_horas<cuarto_cuartil) 

nueva <- nueva %>% 
  mutate(logw = log(y_ingLab_m_ha))

desviacion_estandar_filter <- sd(nueva$y_ingLab_m_ha)
moda <- mfv(nueva$y_ingLab_m_ha)
media <- mean(salario_horas_noNA)
  
#Usar log de w para construir los graficos
hist(df$logw, 
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
rangos_edad <- ifelse(nueva$age >=18 & nueva$age <=30, 1, NA)
rangos_edad <- ifelse(nueva$age > 30 & nueva$age <=50, 2, rangos_edad)
rangos_edad <- ifelse(nueva$age > 50 & nueva$age <=70, 3, rangos_edad)
rangos_edad <- ifelse(nueva$age > 70 & nueva$age <=91, 4, rangos_edad)
label_edad <- c("18-30", "31-50", "51-70", "71-91")
rangos_edad <- factor(rangos_edad, levels = 1:4, labels = label_edad)

par(cex.axis = 0.8) 
boxplot(nueva$logw ~ rangos_edad,
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

#Imputación de NAs con la moda 
df$y_ingLab_m_ha <- ifelse(is.na(df$y_ingLab_m_ha), media, df$y_ingLab_m_ha)
#conteo de cuantas obs estan por encima!!!!!
std_nueva <- sd(df$y_ingLab_m_ha) #Nueva varianza 
write_csv(df, file = "BaseImputada.csv") 

#Seccion 
#1. Decisiones que hemos tomado (variable de w, ocupados, poner como NA el valor extremo)
#2. Cuantos obs hay. Valores raros (el grueso de la datos se concentra en un rango y eso hace que haya valores atipicos) ¿Cuantos? Graficos. Resumen por cuartiles  
#3. Estadisticas descriptivas antes de la imputacion
#4. NAs por media por el tema de la varianza (discutir por que no por la moda)
#5. Como se ve la base completa. Estadisticas descriptivas despues de la imputacion


#Seleccionar variables. 

primer_cuartil <- quantile(salario_horas, 0.25)
segundo_cuartil <- quantile(salario_horas, 0.50)
tercer_cuartil <- quantile(salario_horas, 0.95)
cuarto_cuartil <- quantile(salario_horas, 0.99, na.rm = TRUE)



df %>%
  filter(salario_horas<cuarto_cuartil) %>% 
  ggplot()+geom_histogram(aes(x= salario_horas))


quienes <- df %>%
  filter(salario_horas>=cuarto_cuartil)
