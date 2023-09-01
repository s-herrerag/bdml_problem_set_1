library(pacman)
p_load(rvest, tidyverse)

problem_set_url <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html')
problem_set_html <- read_html(problem_set_url)

xpath_tabla_1 <- "/html/body/table"

tabla_interes <- problem_set_html%>%
  html_nodes(xpath=xpath_tabla_1)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_2 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html')
problem_set_html_2 <- read_html(problem_set_url_2)

xpath_tabla_2 <- "/html/body/table"

tabla_interes_2 <- problem_set_html_2%>%
  html_nodes(xpath=xpath_tabla_2)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_3 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html')
problem_set_html_3 <- read_html(problem_set_url_3)

xpath_tabla_3 <- "/html/body/table"

tabla_interes_3 <- problem_set_html_3%>%
  html_nodes(xpath=xpath_tabla_3)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_4 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html')
problem_set_html_4 <- read_html(problem_set_url_4)

xpath_tabla_4 <- "/html/body/table"

tabla_interes_4 <- problem_set_html_4%>%
  html_nodes(xpath=xpath_tabla_4)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_5 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html')
problem_set_html_5 <- read_html(problem_set_url_5)

xpath_tabla_5 <- "/html/body/table"

tabla_interes_5 <- problem_set_html_5%>%
  html_nodes(xpath=xpath_tabla_5)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_6 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html')
problem_set_html_6 <- read_html(problem_set_url_6)

xpath_tabla_6 <- "/html/body/table"

tabla_interes_6 <- problem_set_html_6%>%
  html_nodes(xpath=xpath_tabla_6)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_7 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html')
problem_set_html_7 <- read_html(problem_set_url_7)

xpath_tabla_7 <- "/html/body/table"

tabla_interes_7 <- problem_set_html_7%>%
  html_nodes(xpath=xpath_tabla_7)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_8 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html')
problem_set_html_8 <- read_html(problem_set_url_8)

xpath_tabla_8 <- "/html/body/table"

tabla_interes_8 <- problem_set_html_8%>%
  html_nodes(xpath=xpath_tabla_8)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_9 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html')
problem_set_html_9 <- read_html(problem_set_url_9)

xpath_tabla_9 <- "/html/body/table"

tabla_interes_9 <- problem_set_html_9%>%
  html_nodes(xpath=xpath_tabla_9)%>%
  html_table()%>%
  as.data.frame()

problem_set_url_10 <- ('https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html')
problem_set_html_10 <- read_html(problem_set_url_10)

xpath_tabla_10 <- "/html/body/table"

tabla_interes_10 <- problem_set_html_10%>%
  html_nodes(xpath=xpath_tabla_10)%>%
  html_table()%>%
  as.data.frame()

tabla_final<-bind_rows(tabla_interes, tabla_interes_2, tabla_interes_3, tabla_interes_4, tabla_interes_5, tabla_interes_6, tabla_interes_7, tabla_interes_8, tabla_interes_9, tabla_interes_10)

write_csv(tabla_final, file = "datosTaller1.csv") 