##############################################################
#       Big Data y Machine Learning                          #
#       Taller 1 - Punto 4                                   #
##############################################################

#-------------------------------------------
# Load packages
pkg <- list("dplyr", "readr", "tidyverse","rio","stargazer", "boot")
lapply(pkg, require, character.only = T)
rm(pkg)

# Clean environment
rm(list = ls())
#-------------------------------------------

# Load data ---------------------------------
data <- read_csv("stores/geih_scraped.csv")

# Item a ------------------------------------
data<- data[!is.na(data$y_ingLab_m_ha),]
data<- data[!is.na(data$maxEducLevel),]

# Regresión unconditional wage gap
data$logwage <- log(data$y_ingLab_m_ha)
data$female <- ifelse(data$sex == 1, 0, 1)
reg1 <- lm(logwage ~ female, data = data)

# Item b -------------------------------------

##FWL
data <- data %>% mutate(femaleResidF = lm(female ~ maxEducLevel + oficio + formal + age + sizeFirm, data)$residuals)
data <- data %>% mutate(WageResidF=lm(logwage~maxEducLevel + oficio + formal + age + sizeFirm, data)$residuals)
reg2 <-lm(WageResidF~femaleResidF,data)

reg3 <- lm(logwage~female+maxEducLevel + oficio + formal + age + sizeFirm, data)

stargazer(reg1,reg2,reg3,type="text",digits=7)

##FWL with boostrap




