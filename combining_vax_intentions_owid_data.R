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
    filter(date < "2022-02-27") %>% 
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
# vax_intentions %>% 
#     select(country = X.1,
#            august = `Total Agree - August 2020`,
#            october = `Total Agree - October 2020`) %>% 
#     inner_join(
#         read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv") %>% 
#             select(date, 
#                    country = location,
#                    partially_vax = people_vaccinated_per_hundred, 
#                    fully_vax = people_fully_vaccinated_per_hundred) %>% 
#             drop_na() %>% 
#             group_by(country) %>% 
#             slice_max(date) %>% 
#             ungroup() %>% 
#             arrange(desc(fully_vax)) 
#         )

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
#Scatter
joined_data %>% 
    ggplot(aes(october, fully_vax)) +
    geom_point() +
    geom_text_repel(aes(label = country),
                    min.segment.length = .1) +
    scale_x_continuous(limits = c(25,100)) +
    scale_y_continuous(limits = c(25,100)) +
    geom_abline(intercept = 0, slope = 1) +
    theme_classic() +
    labs(title = "How do are intentions to be vaccinated compare to actual vaccinations by country",
         y = "Percent of Population Fully Vaccinated<br> week of 21 Feb 2022",
         x = "Percentage of COVID-19 vacination<br>intentions as of Oct 2020"
    ) +
    theme(plot.title = element_textbox_simple(family = title_name,
                                              size = 20,
                                              margin = margin(b=15)),
          axis.title.y = element_markdown(family = body,
                                          margin = margin(r = 10)),
          axis.title.x = element_markdown(family = body, 
                                          margin = margin(t = 10)))
#Dot Plot
joined_data %>% 
    mutate(
        intention_diff = fully_vax - october,
        country = country %>% fct_reorder(-intention_diff)) %>% 
    ggplot(aes(intention_diff, country)) +
    geom_point(size = 3) +
    geom_vline(xintercept = 0) +
    theme_classic() +
    labs(title = "Which countries are lagging to meet thier stated vaccination intentions") +
    theme(plot.title = element_textbox_simple(margin = margin(t=10, b=20)))

#bar Plot
joined_data %>% 
    mutate(
        intention_diff = fully_vax - october,
        country = country %>% fct_reorder(-intention_diff),
        highlight_data = if_else(intention_diff < 0, 1, 0),
        label_diff = scales::percent(intention_diff/100,
                                     accuracy = 0.1)) %>% 
    ggplot(aes(intention_diff, country, fill = highlight_data)) +
    geom_point(show.legend = FALSE) +
    geom_text(aes(label = label_diff),
             nudge_x = 2.5) +
    geom_vline(xintercept = 0) +
    coord_cartesian(clip = "off") +
    scale_fill_gradient(low = primary,
                        high = highlight) +
    theme_classic() +
    labs(title = "Which countries are lagging to meet thier stated vaccination intentions",
         x = "Difference in stated intentions (Oct 2020) and actual vaccinations (Feb 2022)",
         y = NULL) +
    theme(plot.title = element_textbox_simple(margin = margin(t=10, b=20),
                                              size = 28, 
                                              family = title_name),
          plot.margin = margin(l=10, t=10,b=10,r=10),
          axis.title.x = element_markdown(margin = margin(t=10)))

# slope plot 
joined_data %>% 
    select(country, 
           actual = fully_vax, 
           intention = october) %>% 
    pivot_longer(-country, names_to = "status", values_to = "percent") %>%
    mutate(status = factor(status, 
                           levels = c("intention","actual"))) %>% 
    ggplot(aes(status, percent, group = country)) +
    geom_line()



#dumbell plot
joined_data %>% 
    mutate(country = country %>% as_factor() %>% fct_reorder(-fully_vax)) %>% 
    ggplot(aes(y=country)) +
    geom_segment(aes(fully_vax, xend = october, yend = country),
                 arrow = arrow(ends = "first",
                               type = "closed",
                               length = unit(0.13, "inches"),
                               angle = 20),
                 color = primary) +
    geom_point(aes(october), 
               color = "#2c3e50",
               size = 2) +
    theme_classic()
   
    
    
    
    
    

joined_data %>% 
    select(country, 
           actual = fully_vax, 
           intention = october) %>% 
    mutate(country = country %>% fct_reorder(-actual)) %>% 
    pivot_longer(-country, names_to = "status", values_to = "percent") %>%
    ggplot(aes(percent, country, group = country, color = status)) +
    geom_path(arrow = arrow(ends = "first",
                            type = "closed",
                            length = unit(0.1, "inches"),
                            angle = 20)
                           ) +
    theme_classic() +
    theme(legend.position = "top")
