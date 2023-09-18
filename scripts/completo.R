##############################################################
#       Big Data y Machine Learning                          #
#       Taller 1 - Código completo                           #
##############################################################

#-------------------------------------------
# Load packages
pkg <- list("dplyr", "readr", "tidyverse", "rio", "stargazer", "boot", "rvest", "xml2")
lapply(pkg, require, character.only = T)
rm(pkg)

# Clean environment
rm(list = ls())
#-------------------------------------------

############################################
#Web Scrapping
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
write.csv(final_data, "../bdml_problem_set_1/stores/geih.csv")








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

boot(base, fn, R = 1000)





