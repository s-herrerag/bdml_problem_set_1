rm(list=ls())

library(pacman)
p_load(rvest, tidyverse)

problem_set_url <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html')
problem_set_html <- read_html(problem_set_url)

problem_set_html%>%
  html_table()

xpath_tabla_1 <- "/html/body/div/div/div[2]/div/table"

tabla_interes <- problem_set_html%>%
  html_nodes(xpath=xpath_tabla_1)%>%
  html_table()%>%
  as.data.frame()

tabla_interes