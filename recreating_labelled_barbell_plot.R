library(tidyverse)
library(readxl)
library(glue)
library(tidyquant)
library(ggtext)
library(showtext)
library(ggrepel)
library(glue)




#MCCS Colors
base_color <- "#1A2D5A"
primary_color <- "#C31F31"
secondary_color <- "#9D8738"


#add in goggle fonts

font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "lato", "Lato")
font_add_google(family = "montserrat", "Montserrat")


#function used to tell the code below use the above fonts
showtext_auto()

COVIDVAX <- read_excel("COVIDVAX.xlsx")

# Set Colors 
base <- "#FFFFFF"    
primary <- "#AAAAAA"
secondary <- "#000000"
highlight <- "#B51300"

# Set Text
title_name <- "patua-one"
body <-  "montserrat"


data_scatter <- COVIDVAX %>% 
    rename(country = X.1, 
           august = 'Total Agree - August 2020',
           october = 'Total Agree - October 2020') %>% 
    filter(country != "Total") %>% 
    mutate(country = recode(country, 
                            "South Korea" = "S.Korea",
                            "South Africa" = "S. Africa",
                            "United Kingdom" = "UK",
                            "United States" = "USA"),
           highlight_color = if_else(august > october, 1, 0))


legend <- tibble(x = c(85, 95), 
                 y = c(97, 92),
                 label = c("Increasing\nintention", 
                           "Decreasing\nintention"))

data_scatter %>%  
    ggplot(aes(august, october, label = country, color = highlight_color)) +
    geom_abline(slope = 1, 
                intercept = 0,
                color = primary, 
                size = 0.25) +
    geom_point(show.legend = FALSE,
               size = 2) +
    geom_text_repel(min.segment.length = 0,
                     max.overlaps = Inf,
                     family = body,
                     size = 3.5) +
    labs(title = "COVID-19 Vaccination Intent is <span style='color:#B51300'>Decreasing</span> Globally",
         x = "Percent willing to receive<br>vaccine in August 2020", 
         y = "Percent willing to receive<br>vaccine in October 2020",
         caption = "<i>Base: 18,526 online aduts aged 16-74 across 15 countries<i><br> Source: Ipsos")+
    coord_fixed(
                clip = "off") +
    scale_y_continuous(breaks = seq(50, 100, 10),
                      lim = c(50,100)) +
    scale_x_continuous(breaks = seq(50,100, 10),
                       lim = c(50,100)) +
    scale_color_gradient(low = primary,
                         high = highlight) +

    theme(text = element_text(family = body,
                              color = primary),
          plot.title.position = "plot",
          plot.title = element_textbox_simple(family = title_name,
                                              face = "bold",
                                              color = secondary,
                                              size = 25,
                                              margin = margin(t=10, b =10)),
          plot.background = element_rect(fill = base),
          panel.background = element_rect(fill = base),
          plot.caption.position = "plot",
          plot.caption = element_markdown(color = primary, 
                                          hjust = 0),
          axis.text = element_text(color = primary,
                                   size = 12,
                                   family = body),
          axis.title = element_text(size = 14,
                                    family = body),
          axis.title.x = element_markdown(margin = margin(t=10, b = 10)),
          axis.title.y = element_markdown(margin = margin(r=10)),
          axis.ticks = element_blank(),
          axis.line = element_line(),
          legend.position = "none"
    ) +
    geom_text(data = legend, 
              aes(x = x, y = y, label = label),
              color = primary,
              lineheight = 1,
              hjust = 0,
              size = 4,
              inherit.aes = F)



    