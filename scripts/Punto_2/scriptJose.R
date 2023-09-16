## Limpieza de datos


# Carga de paquetes y datos necesarios. -----------------------------------

library(pacman)
p_load(this.path, rvest, tidyverse, stargazer)
file_dir <- this.path::here()
setwd(file_dir)

df <- read.csv("../../stores/geih_scraped.csv")


# Creación del objeto del percentil 99 ------------------------------------

p99 <- quantile(df$y_ingLab_m_ha, 0.99, na.rm = TRUE)


# Limpieza de datos por edad, condición de empleados y eliminación datos atípicos--------

df <- df %>%
  filter(age >= 18, p6240 == 1) %>%
  filter(y_ingLab_m_ha < p99)


# Exportamos base de datos final. -----------------------------------------

write_csv(df, file = "datosTaller1_limpios.csv")


# Otros: ------------------------------------------------------------------

# Tabla resumen de NaN's

nanTable <- as.data.frame(colSums(is.na(df)))

# Estadísticas descriptivas base de datos.

estadisticas_descriptivas <- stargazer(df,
  type = "text", min.max = TRUE, mean.sd = TRUE,
  nobs = TRUE, median = TRUE, iqr = FALSE,
  digits = 1, align = T,
  title = "Summary Statistics"
)