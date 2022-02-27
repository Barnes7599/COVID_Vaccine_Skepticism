library(tidyverse)
library(readxl)
library(glue)
library(tidyquant)
library(ggtext)
library(showtext)
library(ggrepel)
library(glue)

#add in goggle fonts
font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "montserrat", "Montserrat")

#function used to tell the code below use the above fonts
showtext_auto()

# Load and Transform the data ----

vax_intentions <- read_excel("COVIDVAX.xlsx")

owid <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
    
owid_df <-  owid %>% 
    select(date, 
           country = location,
           partially_vax = people_vaccinated_per_hundred, 
           fully_vax = people_fully_vaccinated_per_hundred) %>% 
    drop_na() %>% 
    group_by(country) %>% 
    slice_max(date) %>% 
    ungroup() %>% 
    arrange(desc(fully_vax)) 


joined_data <- vax_intentions %>% 
    select(country = X.1,
           august = `Total Agree - August 2020`,
           october = `Total Agree - October 2020`) %>% 
    inner_join(owid_df)

joined_data %>% 
    arrange(desc(fully_vax))


# Combined code ----
vax_intentions %>% 
    select(country = X.1,
           august = `Total Agree - August 2020`,
           october = `Total Agree - October 2020`) %>% 
    inner_join(
        read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
            select(date, 
                   country = location,
                   partially_vax = people_vaccinated_per_hundred, 
                   fully_vax = people_fully_vaccinated_per_hundred) %>% 
            drop_na() %>% 
            group_by(country) %>% 
            slice_max(date) %>% 
            ungroup() %>% 
            arrange(desc(fully_vax)) 
        )

# Set design choices ----

# Set Colors 
base <- "#FFFFFF"    
primary <- "#AAAAAA"
secondary <- "#000000"
highlight <- "#B51300"

# Set Text
title_name <- "patua-one"
body <-  "montserrat"

#Visualize the data ----

joined_data %>% 
    ggplot(aes())
    
