## Primera regresión: Edad


# Carga de paquetes y datos requeridos. -------------------------------------------

library(pacman)
p_load(this.path, rvest, tidyverse, stargazer, ggplot2)
file_dir <- this.path::here()
setwd(file_dir)


# Transformación de variables que exige el modelo. ------------------------

df_clean <- read.csv("../../scripts/Punto_2/BaseFinal.csv") %>%
  mutate(logInc = log(y_ingLab_m_ha), age2 = age^2) ## Arreglar variable salario

df_clean <- df_clean %>%
  mutate(age2 = age^2)


# Regresión ---------------------------------------------------------------

salario.edad.lm <- lm(logInc ~ age + age2, data = df_clean)
stargazer(salario.edad.lm, type = "text")


# Gráfico de dispersión de datos con la curva que describe el modelo --------

ggplot(df_clean, aes(y = logInc, x = age)) +
  geom_point() +
  stat_smooth(
    formula = "y ~ x + I(x^2)", method = "lm", se = TRUE,
    size = 1
  ) +
  theme_bw() +
  labs(
    x = "Edad",
    y = "Logaritmo del Ingreso",
    title = "Perfiles de Edad - Ingreso estimados, implicados por el modelo"
  )

# Punto 3: Bootstrap  -----------------------------------------------------
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