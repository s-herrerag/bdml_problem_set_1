## Limpieza de datos

##Santiago: Graficar boxplots de distribución de ingreso por edad, para ver outliers.
##Santiago: ¿Quitar outliers o high leverage?

library(pacman)
p_load(rvest, tidyverse)

tabla_final <- read.csv("C:\\Users\\PC\\Documents\\datosTaller1.csv")

tabla_final_limpia <- tabla_final %>%
  filter(age >= 18, p6240 == 1) ##¿Qué variable tomamos para ingreso? ¿Cuál nos da un ingreso de alguien "empleado"? ¿Empleado por quién?

write_csv(tabla_final_limpia, file = "datosTaller1_limpios.csv")

p_load(stargazer)


#Incluir NaN
estadisticas_descriptivas <- stargazer(tabla_final_limpia,
  type = "text", min.max = TRUE, mean.sd = TRUE,
  nobs = TRUE, median = TRUE, iqr = FALSE,
  digits = 1, align = T,
  title = "Summary Statistics"
)
