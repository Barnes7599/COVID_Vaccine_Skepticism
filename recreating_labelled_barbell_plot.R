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

COVIDVAX <- read_excel("COVIDVAX.xlsx")



# Set Colors 
base <- "#FFFFFF"    
primary <- "#AAAAAA"
secondary <- "#000000"
highlight <- "#B51300"

# Set Text
title_name <- "patua-one"
body <-  "montserrat"


data_diff <- COVIDVAX %>% 
    rename(country = X.1, 
           august = 'Total Agree - August 2020',
           october = 'Total Agree - October 2020') %>% 
    filter(country != "Total") %>% 
    mutate(country = recode(country, 
                            "South Korea" = "S.Korea",
                            "South Africa" = "S. Africa",
                            "United Kingdom" = "UK",
                            "United States" = "USA"),
           highlight_color = if_else(august > october, 1, 0),
           diff = october - august,
           country = country %>% fct_reorder(diff) %>%  fct_rev())


legend <- tibble(x = c(1, -3.25), 
                 y = c(17,17),
                 label = c("Increasing\nintention", 
                           "Decreasing\nintention"))
no_change <- tibble(x = c(-1.5, -1.5),
                    y = c(4,5),
                    label = c("No Change",
                              "No Change"))

data_diff %>%  
    ggplot(aes(diff, country, color = highlight_color)) +
    # ggplot(aes(diff, country, fill = highlight_color)) +
    geom_vline(xintercept = 0, color = primary) +
    geom_hline(aes(yintercept = country), color = primary, linetype = "dotted", alpha = .5) +
    geom_point(show.legend = FALSE,
               size = 2) +
    # geom_col(show.legend = FALSE,
    #            size = 1) +
    # scale_fill_continuous(high = highlight, low = primary) +
    scale_color_continuous(high = highlight, low = primary) +
    labs(title = "COVID-19 Vaccination Intent is <span style='color:#B51300'>Decreasing</span> Globally",
         x = "Percentage point difference in intention to receive<br>COVID-19 vaccine between August and October 2020", 
         y = NULL,
         caption = "<i>Base: 18,526 online aduts aged 16-74 across 15 countries<i><br> Source: Ipsos")+
    coord_cartesian(xlim = c(-15,5), clip = "off") +
    theme(text = element_text(family = body,
                              color = primary),
          plot.title.position = "plot",
          plot.title = element_textbox_simple(family = title_name,
                                              face = "bold",
                                              color = secondary,
                                              size = 25,
                                              margin = margin(t=10, b =35)),
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
          axis.ticks = element_blank(),
          axis.line = element_line(color = primary),
          legend.position = "none"
    ) +
    geom_text(data = legend, 
              aes(x = x, y = y, label = label),
              color = primary,
              lineheight = 1,
              hjust = 0,
              size = 4,
              inherit.aes = F,
              family = body) 
    #  + geom_label(data = no_change, 
    #           aes(x = x, y = y, label = label),
    #           color = primary,
    #           label.size = 0,
    #           lineheight = 1,
    #           hjust = 0,
    #           size = 4,
    #           inherit.aes = F,
    #           family = body)



