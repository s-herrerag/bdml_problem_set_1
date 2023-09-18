## Primera regresión: Edad


# Carga de paquetes y datos requeridos. -------------------------------------------

library(pacman)
p_load(this.path, rvest, tidyverse, stargazer, ggplot2, boot)
file_dir <- this.path::here()
setwd(file_dir)


# Transformación de variables que exige el modelo. ------------------------

df_clean <- read.csv("../Punto_2/BaseFinal.csv") %>%
  mutate(logInc = log(y_ingLab_m_ha), age2 = age^2)

df_clean <- df_clean %>%
  mutate(age2 = age^2)


# Regresión ---------------------------------------------------------------

salario.edad.lm <- lm(logInc ~ age + age2, data = df_clean)
stargazer(salario.edad.lm, type = "latex",
          digits=6,
          out="outputs/reg_salario_edad.tex")


# Interpretación coeficientes, efectos marginales -------------------------

#20 años
summary(salario.edad.lm)$coefficients[2,1] + (2*summary(salario.edad.lm)$coefficients[3,1]*20)

#40 años

summary(salario.edad.lm)$coefficients[2,1] + (2*summary(salario.edad.lm)$coefficients[3,1]*40)

#60 años

summary(salario.edad.lm)$coefficients[2,1] + (2*summary(salario.edad.lm)$coefficients[3,1]*60)

# Gráfico de dispersión de datos con la curva que describe el modelo --------

age_logInc_plot <- ggplot(df_clean, aes(y = logInc, x = age)) +
  geom_point() +
  stat_smooth(
    formula = "y ~ x + I(x^2)", method = "lm", se = TRUE,
    linewidth = 1
  ) +
  theme_bw() +
  labs(
    x = "Edad",
    y = "Logaritmo del Ingreso"
  )
ggsave("outputs/reg_age_inc.png", age_logInc_plot, dpi=300)

# Punto 3: Bootstrap  -----------------------------------------------------

#### Funcion bootstrap
age_boot <- function (df, index){
  
  df_reg <-df[index,]
  
  #Regresion para calcular los coeficientes
  reg <- lm(formula = logInc ~ age + age2,
            data = df_reg)
  b1 <-  reg$coefficients[2]
  b2 <- reg$coefficients[3]
  
  #Estadistico de interes: Maximo analitico de edad
  max_income_age <- -(b1/(2*b2))
  max_income_age
}

#### Estimacion bootstrap
set.seed(415)
bootstrap_results <- boot(df_clean, age_boot, R = 1000)

#Output (Tabla con valores de bootstrap)
output_boot <- broom::tidy(bootstrap_results)
output_boot$mean_boot <- mean(bootstrap_results$t)

output_boot <- output_boot %>%
  mutate(across(c(statistic, bias, std.error, mean_boot), ~round(., 2))) %>%
  mutate(term="Edad de máximos ingresos") %>%
  select(c(term, statistic, mean_boot, bias, std.error))

colnames(output_boot) <- c("", "Estimación muestra original", "Media muestras bootstrap (R=1000)", "Sesgo", "Error estándar")

stargazer(output_boot, summary = FALSE, digits = 2, type = "latex", rownames = F,
          out = "outputs/results_boot.tex")





