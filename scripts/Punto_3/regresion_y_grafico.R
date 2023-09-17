## Primera regresión: Edad


# Carga de paquetes y datos requeridos. -------------------------------------------

library(pacman)
p_load(this.path, rvest, tidyverse, stargazer, ggplot2)
file_dir <- this.path::here()
setwd(file_dir)


# Transformación de variables que exige el modelo. ------------------------

df_clean <- read.csv("../../stores/geih_scraped.csv") %>%
  mutate(logInc = log(y_ingLab_m_ha), age2 = age^2) ## Arreglar variable salario

df_clean <- df_clean %>%
  mutate(age2 = age^2)


# Regresión ---------------------------------------------------------------

salario.edad.lm <- lm(logInc ~ age + age2, data = df_clean)
stargazer(salario.edad.lm, type = "")


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