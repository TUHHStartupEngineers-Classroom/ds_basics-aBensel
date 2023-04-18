library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggrepel)
library(maps)

#get data
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")%>%
  mutate(mort_rate = total_deaths/population)%>%
  group_by(location) %>% summarise(last_mort_rate = last(mort_rate))%>%
  select(location, last_mort_rate)%>%
  mutate(location = case_when(
    
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
    
  )) %>%
  distinct()

world <- map_data("world")

covid_coord <- merge(x=world,y=covid_data_tbl, by.x = "region", by.y = "location")%>%
  select(region, long,lat,last_mort_rate)


#plot
#canvas
covid_coord %>% ggplot() + 

  #geometries
  geom_map(aes(x = long, y = lat, map_id = region, fill = last_mort_rate), map = world) +

  #formatting
  scale_fill_continuous(labels = scales::percent_format(accuracy = 0.001), low = "firebrick1", high = "darkred") +

  labs(
    title = "Confirmed COVID-19 deaths relative to the size of the population",
    subtitle = "Around 3 Million confirmed Covid-19 deaths worldwide",
    caption = "Date: 05/01/2021") +
  
  
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank()) +
  
  theme(title = element_text(face = "bold", color = "black"),
        legend.position  = "right")
  