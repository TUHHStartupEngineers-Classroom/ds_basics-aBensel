library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggrepel)

# get data and wrangle it
covid_data_tbl <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>%
  select(location, date, total_cases) %>%
  filter(location == "Germany" | location == "United Kingdom" | location == "France" | location == "Spain" | location == "United States")%>%
  mutate(cases_dec = scales::dollar(total_cases, big.mark = ".", 
                                           decimal.mark = ",", 
                                           prefix = "", 
                                           suffix = ""))

#plot data

#canvas
covid_data_tbl %>%
  ggplot(aes(x = date, y = total_cases, color = location)) +
  
# Geometries
  geom_line(size = 1) +
  
  geom_label_repel(aes(x=date, y=total_cases, label=cases_dec) , 
                   data = covid_data_tbl %>% slice(which.max(total_cases)),
                   vjust = 0.5, 
                   hjust = 0.5,color = "red")+

# Formatting
  expand_limits(y = 0) +
  
  scale_y_continuous(labels = scales::dollar_format(scale = 1/1e6,
                                                    prefix = "",
                                                    suffix = "M")) +
  
  scale_x_date(date_labels = "%B %Y", 
               date_breaks = "1 month", 
               expand = c(0,NA)) +
  

  labs(
    title = "COVID-19 confirmed cases worldwide",
    subtitle = "As of 01/05/2021",
    x = "Months",
    y = "Cumulative Cases",
    color = "Country"
  )  +
  
  theme_light() +
  theme(title = element_text(face = "bold", color = "darkblue"),
        legend.position  = "bottom",
        axis.text.x = element_text(angle = 45))