library(tidyverse)
library(caret)
library(dplyr)

rm(list = ls())
setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 1 ")

#Trayendo la base de datos tras el scrape 
df <- read.csv("datos.csv")
df <- df[, -1]

set.seed(1100)
muestra <- sample(c(TRUE, FALSE), replace=TRUE, nrow(df), prob=c(0.7,0.3))
sum(muestra)/nrow(df)
train  <- df[muestra, ] #df donde el index muestra es TRUE
test   <- df[!muestra, ] #df donde el index muestra es FALSE 
