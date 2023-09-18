library(tidyverse)
library(dplyr)
library(ggplot2)
library(modeest)


rm(list = ls())
file_dir <- this.path::here()
setwd(file_dir)
#setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 1 ")

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

geih_clean <- read.csv("BaseFinal.csv")

geih_clean$female <- NA 
geih_clean$female <- ifelse(geih_clean$sex == 0, 1,0)

geih_clean <- geih_clean %>% 
  mutate(logw=log(y_ingLab_m_ha)) %>%
  mutate(totalHoursWorked2=totalHoursWorked^2) %>%
  mutate(age2=age^2) %>%
  select(y_ingLab_m_ha, 
         logw, 
         age, 
         age2, 
         sex, 
         female, 
         clase, 
         depto, 
         formal, 
         maxEducLevel, 
         oficio, 
         totalHoursWorked, 
         totalHoursWorked2, 
         ocu, 
         p6240)

colnames(geih_clean)[colnames(geih_clean) == "y_ingLab_m_ha"] <- "W"

salario_horas <- geih_clean$W

prueba_ocu <- geih_clean$ocu - geih_clean$p6240
summary(prueba_ocu) #Todos los que tienen p6240 son ocupados, no hay cosas raras. En cambio, hay personas que son ocupadas
                    #que declaran utilizar su tiempo en cosas diferentes a trabajar... 


# Gráficos ----------------------------------------------------------------

HistW <- ggplot(geih_clean) + 
  geom_histogram(aes(x=W), color = "black", fill = "grey") +
  scale_y_continuous(breaks = seq(0, 4000, by = 1000)) + 
  scale_x_continuous(breaks = seq(0, 70000, by = 10000), limits = c(NA, 70000)) + 
  labs(y="Frecuencia", x="(w)") +
  theme_classic()
ggsave("HistW.png", plot = HistW, path = "../../graphics", dpi = 500)  

DensidadW <- ggplot(geih_clean) + 
  geom_density(aes(x=W), color = "black", fill = "grey") +
  scale_x_continuous(breaks = seq(0, 60000, by = 10000), limits = c(NA, 70000)) + 
  labs(y="Densidad", x="(w)") +
  theme_classic()
ggsave("DensidadW.png", plot = DensidadW, path = "../../graphics", dpi = 500) 

plot(density(salario_horas))

hist(geih_clean$age) #El max son 91 años 

geih_clean$rangos_edad <- NA
geih_clean$rangos_edad <- ifelse(geih_clean$age >=18 & geih_clean$age <=30, 1, NA)
geih_clean$rangos_edad <- ifelse(geih_clean$age > 30 & geih_clean$age <=50, 2, geih_clean$rangos_edad)
geih_clean$rangos_edad <- ifelse(geih_clean$age > 50 & geih_clean$age <=70, 3, geih_clean$rangos_edad)
geih_clean$rangos_edad <- ifelse(geih_clean$age > 70 & geih_clean$age <=91, 4, geih_clean$rangos_edad)
label_edad <- c("18-30", "31-50", "51-70", "71-91")
geih_clean$rangos_edad <- factor(geih_clean$rangos_edad, levels = 1:4, labels = label_edad)

BoxEdad <- ggplot(geih_clean) +
  geom_boxplot(aes(x=factor(rangos_edad), y=logw), color = "black") +
  labs(y= "log(w)", x="Rangos de edad") +
  theme_classic()
ggsave("BoxEdad.png", plot = BoxEdad, path = "../../graphics", dpi = 500)  

BoxFemale <- ggplot(geih_clean) +
  geom_boxplot(aes(x=factor(female), y=logw), color = "black") +
  labs(y= "log(w)", x="Female") +
  theme_classic()
ggsave("BoxFemale.png", plot = BoxFemale, path = "../../graphics", dpi = 500)  