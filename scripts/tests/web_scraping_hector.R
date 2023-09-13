library(tidyverse); library(rvest)

rm(list = ls())
setwd("/Users/hectorsegura/Documentos/Big Data & ML/Taller 1 ")
getwd()

#Probando con la primera tabla por separado
HTML1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html")
#Se observa que la tabla está dentro de "w3-include-html" de la página 'pages/geih_page_1.html' 

tablas_url<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")

df<-data.frame()
for (url in tablas_url){
  print(url)
  temp<-read_html(url) %>% html_table() 
  temp<-as.data.frame(temp[[1]])
  df<-rbind(df, temp)
  df <- df[, -1]
}

write_csv(df, file = "datos.csv") 