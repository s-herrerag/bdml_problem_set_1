# Punto 2: Bootstrap  -----------------------------------------------------
require(pacman)
p_load(tidyverse, boot)

#### Cargar Datos
df_clean <- read.csv("../stores/geih.csv")

#### Funcion bootstrap
age_boot <- function (df, index){
  
  df_reg <-df[index,]
  
  #Regresion para calcular los coeficientes
  reg <- lm(formula = income ~ age + age2,
            data = df_reg)
  b1 <-  reg$coefficients[1]
  b2 <- reg$coefficients[2]
  
  #Estadistico de interes: Maximo analitico de edad
  max_income_age <- -(b1/2*b2)
  max_income_age
}

#### Estimacion bootstrap
bootstrap_results <- boot(df_clean, age_boot, R = 1000)






