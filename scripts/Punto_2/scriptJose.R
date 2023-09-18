## Limpieza de datos


# Carga de paquetes y datos necesarios. -----------------------------------

library(pacman)
p_load(this.path, rvest, tidyverse, stargazer, xtable)
file_dir <- this.path::here()
setwd(file_dir)

df <- read.csv("../../stores/geih_scraped.csv")


# Gráfico de dispersión inicial, variable de interés. ---------------------

plot(df$y_ingLab_m_ha)

# Creación del objeto del percentil 99 ------------------------------------

p99 <- quantile(df$y_ingLab_m_ha, 0.99, na.rm = TRUE)


# Limpieza de datos por edad, condición de empleados y eliminación datos atípicos--------

df <- df %>%
  filter(age >= 18, p6240 == 1) %>%
  filter(y_ingLab_m_ha < p99)


# Exportamos base de datos final. -----------------------------------------

write_csv(df, file = "datosTaller1_limpios.csv")


# Otros: ------------------------------------------------------------------

#Selección de las variables de interés

df <- df %>% 
  select("y_ingLab_m_ha", 
         "age", 
         "sex", 
         "clase", 
         "depto", 
         "formal", 
         "maxEducLevel", 
         "oficio", 
         "totalHoursWorked")

# Tabla resumen de NaN's

nanTable <- as.data.frame(colSums(is.na(df)))
print(xtable(nanTable, type = "latex"), file = "outputs/nan_table.tex")

# Estadísticas descriptivas base de datos.

#Hacemos la tabla

descriptive_statistics <- stargazer(df,
  type = "latex", min.max = TRUE, mean.sd = TRUE,
  nobs = TRUE, median = TRUE, iqr = FALSE,
  digits = 1, align = T,
  title = "Summary Statistics",
  covariate.labels = c("Ingreso por Hora", "Edad", "Sexo", "Clase", "Departamento", "Formalidad", "Máximo Nivel Educativo Alcanzado", "Oficio", "Total de Horas Trabajadas"),
  out="outputs/summ_statistics.tex"
)