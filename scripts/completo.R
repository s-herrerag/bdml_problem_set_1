##############################################################
#       Big Data y Machine Learning                          #
#       Taller 1 - Código completo                           #
##############################################################

#-------------------------------------------
# Load packages
pkg <- list("dplyr", "readr", "tidyverse", "rio", "stargazer", "boot", "rvest", "xml2",
            "this.path", "tidymodels", "xtable")
lapply(pkg, require, character.only = T)
rm(pkg)

# Clean environment
rm(list = ls())
#-------------------------------------------

############################################
#Web Scraping
############################################

# Main web
url_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
main <- read_html(url_base)

# Links to the data
links_data <- (main %>% html_nodes("a") %>% xml_attr("href"))[seq(from = 7, to = 16)]
urls <- paste0(url_base, links_data)

# Extract data of each link
data_frame_list <- list()

for (url in urls) {
  page <- read_html(url)
  link_node <- page %>%
    html_nodes("div.col-md-9") %>%
    html_nodes("div") %>%
    xml_attr("w3-include-html")
  url_page <- paste0(url_base, link_node)
  table <- read_html(url_page) %>%
    html_table() %>%
    as.data.frame()
  data_frame_list[[url]] <- table
}

# Merge data
final_data <- bind_rows(data_frame_list)

file_dir <- this.path::here()
setwd(file_dir)
write.csv(final_data, "../bdml_problem_set_1/stores/geih.csv")

############################################
#Punto 2
############################################

rm(list = ls())
file_dir <- this.path::here()
setwd(file_dir)

#Trayendo la base de datos tras el scrape 
df <- read.csv("Punto_2/datos.csv")
df <- df[, -1]

#Limpieza de los datos y creación de variables ------------------------------

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


# Tablas ------------------------------------------------------------------

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

# Tabla resumen de NaN's --------------------------------------------------

nanTable <- as.data.frame(colSums(is.na(df)))

# Estadísticas descriptivas base de datos ---------------------------------

descriptive_statistics <- stargazer(df,
                                    type = "latex", min.max = TRUE, mean.sd = TRUE,
                                    nobs = TRUE, median = TRUE, iqr = FALSE,
                                    digits = 1, align = T,
                                    title = "Summary Statistics",
                                    covariate.labels = c("Ingreso por Hora", "Edad", "Sexo", "Clase", "Departamento", "Formalidad", "Máximo Nivel Educativo Alcanzado", "Oficio", "Total de Horas Trabajadas")
)

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


############################################
#Punto 3
############################################

# Carga de paquetes y datos requeridos. -------------------------------------------

file_dir <- this.path::here()
setwd(file_dir)


# Transformación de variables que exige el modelo. ------------------------

df_clean <- read.csv("../stores/BaseFinal.csv") %>%
  mutate(logInc = log(y_ingLab_m_ha), age2 = age^2)

df_clean <- df_clean %>%
  mutate(age2 = age^2)


# Regresión ---------------------------------------------------------------

salario.edad.lm <- lm(logInc ~ age + age2, data = df_clean)
stargazer(salario.edad.lm, type = "latex",
          digits=6,
          out="../views/tables/reg_salario_edad.tex")


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
ggsave("../views/graphs/reg_age_inc.png", age_logInc_plot, dpi=300)

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
          out = "../views/tables/results_boot.tex")


############################################
#Punto 4
############################################

# Item a ------------------------------------
base <- base[!is.na(base$y_ingLab_m_ha), ]
base <- base[!is.na(base$maxEducLevel), ]

# Regresión unconditional wage gap
base$logwage <- log(base$y_ingLab_m_ha)
base$female <- ifelse(base$sex == 1, 0, 1)
reg1 <- lm(logwage ~ female, data = base)

# Item b -------------------------------------
base$age2 <- (base$age)^2

## FWL
base <- base %>% mutate(femaleResidF = lm(female ~ maxEducLevel + relab + formal + age + age2 + sizeFirm, base)$residuals)
base <- base %>% mutate(WageResidF = lm(logwage ~ maxEducLevel + relab + formal + age + age2 + sizeFirm, base)$residuals)
reg2 <- lm(WageResidF ~ femaleResidF, base)

stargazer(reg1, reg2, type = "latex", digits = 6, covariate.labels = c("Female", "Female Resid"), dep.var.labels = c("Wage", "Wage"), title = "Modelos wage gap", omit = c("Constant"))

## FWL with boostrap
fn <- function(data, index) {
  data <- data %>% mutate(femaleResidF = lm(female ~ maxEducLevel + relab + formal + age + age2 + sizeFirm, data = data)$residuals)
  data <- data %>% mutate(WageResidF = lm(logwage ~ maxEducLevel + relab + formal + age + age2 + sizeFirm, data = data)$residuals)
  coef(lm(WageResidF ~ femaleResidF, data = data, subset = index))[2]
}

set.seed(415)
boot(base, fn, R = 1000)

############################################
#Punto 5
############################################

# Libraries and data ------------------------------------------------------
file_dir <- this.path::here()
setwd(file_dir)

geih_clean <- read.csv("../stores/BaseFinal.csv")

#We also need to verify the factor data
geih_clean <- geih_clean %>%
  mutate(across(c(depto, oficio, maxEducLevel), as.factor)) %>%
  mutate(logw=log(y_ingLab_m_ha)) %>%
  mutate(totalHoursWorked2=totalHoursWorked^2) %>%
  mutate(age2=age^2) %>%
  select(c(logw, age, age2, sex, depto, formal, maxEducLevel, oficio, totalHoursWorked, totalHoursWorked2)) %>%
  drop_na()


# a) Sample split ---------------------------------------------------------
set.seed(613)

# Training with 70%
geih_split <- initial_split(geih_clean, prop = .7)

# Dataframes
train <- training(geih_split)
test  <- testing(geih_split)


# b) Models and alternative specifications------------------------------------------------------------------
#Vars to add: formal, maxEducLevel, oficio, totalHoursWorked

rec_age <- recipe(logw ~ age + age2, data = train)
rec_gender <- recipe(logw ~ sex, data = train)

rec_ext1 <- recipe(logw ~ age + age2 + sex, data=train)
rec_ext2 <- recipe(logw ~ age + age2 + sex, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext3 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext4 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext5 <- recipe(logw ~ age + age2 + sex  + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext6 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ sex:maxEducLevel + sex:oficio ) %>%
  step_dummy(all_factor_predictors())
rec_ext7 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ sex:maxEducLevel:+ sex:oficio) %>%
  step_dummy(all_factor_predictors())
rec_ext8 <- recipe(logw ~ age + age2 + sex +  formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ totalHoursWorked:maxEducLevel) %>%
  step_dummy(all_factor_predictors())
rec_ext9 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ totalHoursWorked:maxEducLevel + oficio:maxEducLevel) %>% 
  step_dummy(all_factor_predictors())

#Estimate fits
list_recipes <- list(rec_age, rec_gender, rec_ext1, rec_ext2, rec_ext3, 
                     rec_ext4, rec_ext5, rec_ext6, rec_ext7, rec_ext8, rec_ext9)

#Lapply with workflows
fit_tidy_model <- function(x, df=train) {
  lm_model <- linear_reg()
  
  wflow <- workflow() %>% 
    add_model(lm_model) %>% 
    add_recipe(x)
  
  fitted_model <-  wflow %>% 
    fit(data = df)
  
  fitted_model
}

list_workflows <- lapply(list_recipes, function(x){fit_tidy_model(x, train)})

#Lapply with predictions
predict_from_workflow <- function(w, df_test=test) {
  predictions <- predict(w, new_data = df_test) %>% 
    bind_cols(df_test) %>% 
    mutate(Error = exp(logw) - exp(.pred)) #Error en pesos 
  
  predictions
}

rmse_from_predict <- function(pred) {
  test_rmse <- rmse(pred, truth = logw, estimate = .pred)
  test_rmse$.estimate
}

list_predictions <- lapply(list_workflows, function (w){predict_from_workflow(w, test)})
list_rmse <- lapply(list_predictions, function (pred){rmse_from_predict(pred)})

###Report RMSE in a table
rmse_df <- data.frame(list_rmse) 

Hist_error <- ggplot(list_predictions[[11]]) + 
  geom_histogram(aes(x=Error), color = "black", fill = "grey") +  
  scale_y_continuous(breaks = seq(0, 1500, by = 150)) + 
  scale_x_continuous(breaks = seq(-10000, 50000, by = 10000), limits = c(NA, 50000)) + 
  ylab("Frecuencia") + 
  xlab("Error de predicción \n (Diferencia entre salario observado y predicho en pesos)") + 
  theme_classic()

ggsave("HistError.png", plot = Hist_error, path = "../views/graphics", dpi = 500)  

summary(list_predictions[[11]]$Error)
std <- sd(list_predictions[[11]]$Error)

# d) LOOCV -------------------------------------------------------------------

loocv_preds_model1 <- vector("numeric", length = nrow(geih_clean))
loocv_preds_model2 <- vector("numeric", length = nrow(geih_clean))

for (i in seq_len(nrow(geih_clean))) {
  loo_data <- geih_clean[-i, ]
  
  #Model 1
  loo_fit1 <- list_workflows[[10]] %>% fit(data = loo_data)
  pred1 <- predict(loo_fit1, new_data = slice(geih_clean, i))$.pred
  loocv_preds_model1[i] <- pred1
  
  #Model 2
  loo_fit2 <- list_workflows[[11]] %>% fit(data = loo_data)
  pred2 <- predict(loo_fit2, new_data = slice(geih_clean, i))$.pred
  loocv_preds_model2[i] <- pred2
  
  print(paste0("Iteration: ",i))
}

pred1_dataset_loocv <-bind_cols(geih_clean$logw, loocv_preds_model1)
pred2_dataset_loocv <-bind_cols(geih_clean$logw, loocv_preds_model2)

loocv_rmse1 <- rmse(pred1_dataset_loocv, truth = ...1, estimate = ...2)
loocv_rmse2 <- rmse(pred2_dataset_loocv, truth = ...1, estimate = ...2)

rmse_loocv_df <- data.frame(modelos = c("Modelo 10", "Modelo 11"), mse= c(loocv_rmse1$.estimate, loocv_rmse2$.estimate))
colnames(rmse_loocv_df) <- c("", "MSE de LOOCV")
stargazer(rmse_loocv_df, summary = F, rownames = F,
          out="../views/tables/rmse_loocv.tex")



