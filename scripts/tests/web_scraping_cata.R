############################################################## 
#       Big Data y Machine Learning                          #
#       Taller 1                                             #
##############################################################
#-------------------------------------------
#Load packages
library(rvest); library(dplyr); library(xml2)

#Clean environment
rm(list = ls())
#-------------------------------------------

#Main web
url_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/"
main <- read_html(url_base)

#Links to the data
links_data <- (main %>% html_nodes("a") %>% xml_attr("href"))[seq(from = 7, to = 16)]
urls <- paste0(url_base,links_data)

#Extract data of each link
data_frame_list <- list()

for (url in urls) {
  page <- read_html(url) 
  link_node <- page %>% html_nodes("div.col-md-9") %>% html_nodes("div") %>% xml_attr("w3-include-html") 
  url_page <- paste0(url_base, link_node)
  table <- read_html(url_page) %>% html_table() %>% as.data.frame()
  data_frame_list[[url]] <- table
}

#Merge data
final_data <- bind_rows(data_frame_list)
write.csv(final_data, "../bdml_problem_set_1/stores/geih.csv")
