library(tidyverse)
library(caret)
library(dplyr)
library(stargazer)

rm(list = ls())
setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 1 ")

#Trayendo la base de datos tras el scrape 
df <- read.csv("BaseImputada.csv")

df <- df %>% 
  mutate(logW = log(y_ingLab_m_ha)) %>% 
  mutate(age2 = (age)^2)

df$female <- NA 
df$female <- ifelse(df$sex == 0, 1,0)

df <- df %>% 
  select(y_ingLab_m_ha, logW, age, age2, female)

colnames(df)[colnames(df) == "y_ingLab_m_ha"] <- "W"

set.seed(1100)
muestra <- sample(c(TRUE, FALSE), replace=TRUE, nrow(df), prob=c(0.7,0.3))
sum(muestra)/nrow(df)
train  <- df[muestra, ] #df donde el index muestra es TRUE
test   <- df[!muestra, ] #df donde el index muestra es FALSE 

stargazer(data.frame(df), header=FALSE, type='text',title="Variables Included in the Selected Data Set")


