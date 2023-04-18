#libraries
library(dplyr)
library(httr)
library(glue)
library(rvest)
library(stringr)
library(purrr)
library(tibble)
library(tidyverse)
library(jsonlite)

#Universities API function
universities_api <- function(country) {
  url <- modify_url(url = "http://universities.hipolabs.com", path = glue("/search?country={country}"))
  resp <- GET(url)
  stop_for_status(resp)
}

#Argument can be any country. Replace spaces with a plus sign, e.g. in case of United States type "United+States"
resp <- universities_api("Germany")

#get into tibble
uni_tbl <- resp %>% 
  .$content %>% 
  rawToChar() %>% 
  fromJSON()

#reorganize in tibble
output_tbl <- uni_tbl %>% select(name, web_pages) %>% rename(Name = name, Website = web_pages)

#plot
output_tbl %>% head(n = 5) 