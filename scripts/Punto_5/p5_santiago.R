################################
#Punto 5: Predicción del ingreso
################################

# Libraries and data ------------------------------------------------------
require(pacman)
p_load(this.path, tidyverse, tidymodels)
file_dir <- this.path::here()
setwd(file_dir)

geih_clean <- read.csv("../Punto_2/BaseFinal.csv")

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
rec_ext2 <- recipe(logw ~ age + age2 + sex + formal, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext3 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext4 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext5 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_dummy(all_factor_predictors())
rec_ext6 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ sex:maxEducLevel + sex:oficio ) %>%
  step_dummy(all_factor_predictors())
rec_ext7 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ sex:maxEducLevel + sex:oficio) %>%
  step_dummy(all_factor_predictors())
rec_ext8 <- recipe(logw ~ age + age2 + sex + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ totalHoursWorked:maxEducLevel) %>%
  step_dummy(all_factor_predictors())
rec_ext9 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
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

error <- list_predictions[[11]]$Error
quienes <- list_predictions[[11]] %>% 
  filter(error > 0)

# d) LOOCV -------------------------------------------------------------------

loocv_preds_model1 <- vector("numeric", length = nrow(geih_clean))
loocv_preds_model2 <- vector("numeric", length = nrow(geih_clean))

for (i in seq_len(nrow(geih_clean))) {
  loo_data <- geih_clean[-i, ]
  loo_fit <- list_workflows[[11]] %>% fit(data = loo_data)
  pred <- predict(loo_fit, new_data = slice(geih_clean, i))$.pred
  loocv_preds_model1[i] <- pred
  print(paste0("Iteration: ",i))
}

pred1_dataset_loocv <-bind_cols(geih_clean$logw, loocv_preds_model1)
pred2_dataset_loocv <-bind_cols(geih_clean$logw, loocv_preds_model2)

loocv_rmse1 <- rmse(loocv_preds_model1, truth = ...1, estimate = ...2)
loocv_rmse2 <- rmse(loocv_preds_model2, truth = ...1, estimate = ...2)

#Another possibility is estimating the LOOCV using the leverage statistic:

#Estimate the two models with lm on the entire dataset

#Model 11:

model_11_lm <- glm(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2 + totalHoursWorked:maxEducLevel + oficio:maxEducLevel,
                  data=geih_clean)

del <- cv.glm(geih_clean, model_11_lm)$delta

leverages_model_11 <- as.data.frame(hatvalues(model_11_lm))

predictions_model_11 <- predict(model_11_lm, geih_clean)

geih_model_11 <- bind_cols(select(geih_clean, logw), predictions_model_11, leverages_model_11) %>%
  rename(c("pred"="...2", "leverage"="hatvalues(model_11_lm)")) %>%
  mutate(error = logw-pred) %>%
  mutate(influence = (error/(1-leverage))^2) %>%
  mutate(influence = ifelse(influence>100, NA, influence))

mean(geih_model_11$influence, na.rm = T)

lev_test <- broom::augment(model_11_lm)

lev_model11_plt <- ggplot(geih_model_11) +
  geom_point(aes(x=error, y=hatvalues(model_11_lm)))

loocv_model11 <- 

#Model 12

model_12_lm <- lm()
rec_ext9 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ totalHoursWorked:maxEducLevel + oficio:maxEducLevel) %>% 
  step_dummy(all_factor_predictors())
rec_ext10 <- recipe(logw ~ age + age2 + sex + clase + formal + maxEducLevel + oficio + totalHoursWorked + totalHoursWorked2, data=train) %>%
  step_interact(terms = ~ totalHoursWorked:maxEducLevel + oficio:maxEducLevel + formal:clase) %>% 
  step_dummy(all_factor_predictors())

loocv_rmse <- rmse(loocv_preds_model1, truth = ...1, estimate = ...2)

loocv_rmse










