## Primera regresión: Edad

library(pacman)
p_load(rvest, tidyverse, stargazer, ggplot2)

tabla_final_limpia <- read.csv("C:\\Users\\PC\\Documents\\datosTaller1_limpios.csv") %>%
  mutate(logInc = log(y_ingLab_m_ha), age2 = age^2) ##Arreglar variable salario

tabla_final_limpia <- tabla_final_limpia %>%
  mutate(age2 = age^2)

salario.edad.lm <- lm(logInc ~ age + age2, data = tabla_final_limpia)
stargazer(salario.edad.lm, type="text")

ggplot(tabla_final_limpia, aes(y = logInc, x = age)) +
  geom_point() +
  stat_smooth(formula = 'y ~ x + I(x^2)', method = "lm", se = TRUE, 
              size = 1) +
  theme_bw() +
  labs(x = "Age",  
       y = "LogIncome",
       title = "Estimated Age-Earning profiles Implied by the model")

##Falta Bootstrap