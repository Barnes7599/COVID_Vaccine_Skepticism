library(tidyverse)
library(lubridate)

owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

countries <- c("India","China","South Korea","Brazil",
               "Australia", "United Kingdom", "Mexico",
               "Canada", "Germany", "Japan", "South Africa", 
               "Italy", "Spain", "United States", "France") 

owid %>% 
  filter(location %in% c(countries)) %>% 
  select(date, 
         country = location, 
         partially_vax = people_vaccinated_per_hundred, 
         fully_vax = people_fully_vaccinated_per_hundred) %>% 
  group_by(country) %>% 
  drop_na() %>% 
  summarise(date = max(date),
            partially_vax = max(partially_vax),
            fully_vax = max(fully_vax)) %>% 
  ungroup()
