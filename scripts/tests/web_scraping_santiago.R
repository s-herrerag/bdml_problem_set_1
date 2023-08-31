# Libraries ---------------------------------------------------------------
require(pacman)
p_load(rvest, xml2, tidyverse)
script_dir<-this.path::here()
setwd(script_dir)

# Scraping ---------------------------------------------------------------

#Initial website: find links to chunks
root_wb <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/")

links_from_root <- root_wb%>%
  html_elements("a")%>%
  xml_attr("href")

data_chunks_links <- c()
for (link in links_from_root) {
  if (grepl("page", link)==TRUE) {
    chunk_html<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/", link)
    data_chunks_links <- c(data_chunks_links, chunk_html)
  } 
}

#Explore each link to a chunk and extract data to a list of data frames
chunks_data_frames<-list()

for (chunk in data_chunks_links) {
  chunk_wb<-read_html(chunk)
  table_link <- chunk_wb %>%
    html_elements("div") %>%
    xml_attr("w3-include-html")
  table_link <- table_link[!is.na(table_link)]
  table_html <- read_html(paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/", table_link))
  tables<-table_html %>%
    html_table()
  table_df <- as.data.frame(tables[[1]])
  
  #Add to list
  chunks_data_frames[[chunk]]<-table_df
  
  print(paste("Successful ", chunk))
}


# Generate output and export dataset --------------------------------------

geih_complete_df <- bind_rows(chunks_data_frames) %>%
  select(-c("...1"))

write_csv(geih_complete_df, "../../stores/geih_scraped.csv")





