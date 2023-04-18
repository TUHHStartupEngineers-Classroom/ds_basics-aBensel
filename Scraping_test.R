library(dplyr)
library(httr)
library(jsonlite)
library(rvest)
library(stringr)
library(purrr)
library(tibble)



scrape_data <- function(url) {
  
  html_racing_bikes <- read_html(url)
  
  bike_name  <- html_racing_bikes %>%
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    html_text()%>% str_replace_all("[\r\n]" , "") %>% 
    enframe(name = "No.", value = "Name")
  
  bike_price  <- html_racing_bikes %>%
    html_nodes(css = ".catalog-category-bikes__price-title") %>%
    html_text() %>% str_replace_all("[\r\n]" , "") %>% 
    enframe(name = "No.", value = "Price") 
  
  bike_info <- left_join(bike_name,bike_price) %>% select(-("No."))
  }

url= "https://www.rosebikes.de/fahrr%C3%A4der/rennrad"
bike_output<-scrape_data(url)
bike_output