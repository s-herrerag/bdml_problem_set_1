## Primera regresión: Edad

library(pacman)
p_load(rvest, tidyverse, stargazer, ggplot2)

tabla_final_limpia <- read.csv("C:\\Users\\PC\\Documents\\datosTaller1_limpios.csv") %>%
  mutate(logInc = log(p6500), age2 = age^2)

tabla_final_limpia <- tabla_final_limpia %>%
  mutate(age2 = age^2)

salario.edad.lm <- lm(logInc ~ age + age2, data = tabla_final_limpia)
stargazer(salario.edad.lm, type="text")

ggplot(tabla_final_limpia, aes(y = logInc, x = age)) +
  geom_point() +
  stat_smooth(formula = log(y) ~ x + (x^2), method = "lm", se = FALSE, 
              size = 1) +
  theme_bw() +
  labs(x = "Age",  
       y = "LogIncome",
       title = "Estimated Age-Earning profiles Implied by the model")