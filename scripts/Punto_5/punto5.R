################################
#Punto 5: Predicción del ingreso
################################

# Libraries and data ------------------------------------------------------
require(pacman)
p_load(this.path, tidyverse, tidymodels, boot, stargazer)
file_dir <- this.path::here()
setwd(file_dir)

geih_clean <- read.csv("../Punto_2/BaseFinal.csv")

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
#Vars to add: , depto, formal, maxEducLevel, oficio, totalHoursWorked

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

ggsave("HistError.png", plot = Hist_error, path = "../../graphics", dpi = 500)  

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
          out="outputs/rmse_loocv.tex")


#Another possibility is estimating the LOOCV using the leverage statistic:
#Test: Model 10 

model_10_lm <- glm(logw ~ age + age2 + sex  + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2 + totalHoursWorked:maxEducLevel,
                   data=geih_clean)

leverages_model_10 <- as.data.frame(hatvalues(model_10_lm))
predictions_model_10 <- predict(model_10_lm, geih_clean)

geih_model_10 <- bind_cols(select(geih_clean, logw), predictions_model_10, leverages_model_10) %>%
  rename(c("pred"="...2", "leverage"="hatvalues(model_10_lm)")) %>%
  mutate(error = logw-pred) %>%
  mutate(influence = (error/(1-leverage))^2) %>%
  mutate(influence = ifelse(influence>100, NA, influence)) #The issue is that some obs have leverage =1

mean(geih_model_10$influence, na.rm = T)
