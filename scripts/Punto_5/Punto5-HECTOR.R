library(tidyverse)
library(caret)
library(dplyr)
library(stargazer)

rm(list = ls())
setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 1 ")

#Trayendo la base de datos tras el scrape 
df <- read.csv("BaseFinal.csv")

df <- df %>% 
  mutate(logW = log(y_ingLab_m_ha)) %>% 
  mutate(age2 = (age)^2)

df$female <- NA 
df$female <- ifelse(df$sex == 0, 1,0)

df <- df %>% 
  select(y_ingLab_m_ha, logW, age, age2, female, )

colnames(df)[colnames(df) == "y_ingLab_m_ha"] <- "W"

set.seed(1100)
muestra <- sample(c(TRUE, FALSE), replace=TRUE, nrow(df), prob=c(0.7,0.3))
sum(muestra)/nrow(df)
train  <- df[muestra, ] #df donde el index muestra es TRUE
test   <- df[!muestra, ] #df donde el index muestra es FALSE 

stargazer(data.frame(df), header=FALSE, type='text',title="Variables Included in the Selected Data Set")

model_age<-lm(logW~age + age2,data=train)
summary(model_age)
test$model_age<-predict(model_age,newdata = test)
with(test,mean((logW-model_age)^2))

model_female<-lm(logW~female,data=train)
summary(model_female)
test$model_female<-predict(model_female,newdata = test)
with(test,mean((logW-model_female)^2))


#clase 
#oficio
#formal 
#maxEducLevel
#hours_worked


################################
#Punto 5: Predicción del ingreso
################################

# Libraries and data ------------------------------------------------------
require(pacman)
p_load(this.path, tidyverse, ggplot2, tidymodels)
library(tidymodels)
file_dir <- this.path::here()
setwd(file_dir)


#geih_clean <- read.csv("../../stores/geih_scraped.csv")
geih_clean <- read.csv("BaseFinal.csv")


#We also need to verify the factor data
geih_clean <- geih_clean %>%
  mutate(across(c(depto, oficio, maxEducLevel), as.factor)) %>%
  mutate(logw=log(y_ingLab_m_ha)) %>%
  mutate(totalHoursWorked2=totalHoursWorked^2) %>%
  mutate(age2=age^2) %>%
  select(c(logw, age, age2, sex, clase, depto, formal, maxEducLevel, oficio, totalHoursWorked, totalHoursWorked2)) %>%
  drop_na()


# a) Sample split ---------------------------------------------------------
set.seed(613)

# Training with 70%
geih_split <- initial_split(geih_clean, prop = .7)

# Dataframes
train <- training(geih_split)
test  <- testing(geih_split)


# b) Models and alternative specifications------------------------------------------------------------------
#Vars to add: clase, depto, formal, maxEducLevel, oficio, totalHoursWorked

rec_age <- recipe(logw ~ age + age2, data = train)
rec_gender <- recipe(logw ~ sex, data = train)

rec_ext1 <- recipe(logw ~ age + age2 + sex, data=train)
rec_ext2 <- recipe(logw ~ age + age2 + sex + clase, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext3 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext4 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked, data=train) %>%
  step_dummy(all_factor_predictors()) #Segundo mejor 
rec_ext5 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_dummy(all_factor_predictors()) #Mejor
rec_ext6 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ sex:maxEducLevel + sex:oficio) %>%
  step_dummy(all_factor_predictors())
rec_ext7 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ sex:maxEducLevel:clase+ sex:oficio:clase) %>%
  step_dummy(all_factor_predictors())

rec_ext8 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ totalHoursWorked:maxEducLevel) %>%
  step_dummy(all_factor_predictors())

rec_ext9 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ totalHoursWorked:maxEducLevel + oficio:maxEducLevel) %>% #sex no funciona muy bien 
  step_dummy(all_factor_predictors())

rec_ext10 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ totalHoursWorked:maxEducLevel + oficio:maxEducLevel + formal:clase) %>% #sex no funciona muy bien 
  step_dummy(all_factor_predictors())

#Estimate fits
list_recipes <- list(rec_age, rec_gender, rec_ext1, rec_ext2, rec_ext3, 
                     rec_ext4, rec_ext5, rec_ext6, rec_ext7, rec_ext8, rec_ext9, rec_ext10)

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

hist(list_predictions[[11]]$Error)

Hist_error <- ggplot(list_predictions[[11]]) + 
  geom_histogram(aes(x=Error), color = "black", fill = "grey") +  
  scale_y_continuous(breaks = seq(0, 1500, by = 150)) + 
  scale_x_continuous(breaks = seq(-10000, 50000, by = 10000), limits = c(NA, 50000)) + 
  ylab("Frecuencia") + 
  xlab("Error de predicción \n (Diferencia entre salario real y predicho en pesos)") + 
  theme_classic()

ggsave(Hist_error, "/ruta/Nombre.png", dpi = 500) #Carpeta gráficos   

summary(list_predictions[[11]]$Error)
std <- sd(list_predictions[[11]]$Error)

# d) LOOCV -------------------------------------------------------------------
loocv_preds_model1 <- vector("numeric", length = nrow(geih_clean))
loocv_preds_model2 <- vector("numeric", length = nrow(geih_clean))

for (i in seq_len(nrow(geih_clean))) {
  loo_data <- geih_clean[-i, ]
  loo_fit <- list_workflows[[2]] %>% fit(data = loo_data)
  pred <- predict(loo_fit, new_data = slice(geih_clean, i))$.pred
  loocv_preds_model1[i] <- pred
  print(paste0("Iteration: ",i))
}

pred_dataset_loocv <-bind_cols(geih_clean$logw, loocv_preds_model1)

loocv_rmse <- rmse(loocv_preds_model1, truth = ...1, estimate = ...2)

loocv_rmse














