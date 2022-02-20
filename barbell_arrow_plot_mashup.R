
library(tidyverse)
library(readxl)
library(glue)
library(tidyquant)
library(ggtext)
library(showtext)
library(ggrepel)


#add in goggle fonts
font_add_google(family = "josefin-slab","Josefin Slab")
font_add_google(family = "josefin-sans", "Josefin Sans")
font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "lato", "Lato")
font_add_google(family = "montserrat", "Montserrat")

#function used to tell the code below use the above fonts
showtext_auto()

COVIDVAX <- read_excel("~/COVIDVAX.xlsx")



# Mashup ----

data_mashup <- COVIDVAX %>% 
  rename(country = X.1, 
         percent_august = 'Total Agree - August 2020',
         percent_october = 'Total Agree - October 2020') %>% 
  #adjusts percent data labels
  mutate(bump_august = case_when(percent_august <= percent_october ~ 
                                   percent_august - 1.75,
                                 percent_august > percent_october ~ 
                                   percent_august + 1.75),
         bump_october = case_when(percent_august <= percent_october ~ 
                                    percent_october + 1.75,
                                  percent_august > percent_october ~
                                    percent_october - 1.75),
         y_position = rev(1:nrow(.))) %>% 
  mutate(country = recode(country, 
                          "South Korea" = "S.Korea",
                          "South Africa" = "S. Africa",
                          "United Kingdom" = "UK",
                          "United States" = "USA"
  )) %>% 
  filter(country != "Total")



striped_data_mashup <- data_chartr %>% 
  select(country, y_position) %>% 
  mutate(xmin = 50, 
         xmax = 100,
         ymin = y_position - 0.5,
         ymax = y_position + 0.5,
         fill = c(rep(c("a","b"), length.out = nrow(.)))) %>% 
  # If you want to change the top stripe
  # fill = c("c", rep(c("a","b"), length.out = nrow(.)-1))) 
  pivot_longer(cols = c(xmin, xmax), values_to = "x", names_to = "xmin_xmax") %>% 
  select(-xmin_xmax)


data_mashup %>% 
  pivot_longer(cols = -c(country, y_position), 
               names_to = c(".value", "month"), 
               names_sep = "_") %>% 
  drop_na() %>% 
  #   mutate(country = as_factor(country) %>% fct_rev()) %>% 
  ggplot(aes(percent, y_position, color = month, group = y_position)) +
  geom_ribbon(data = striped_data_mashup,
              aes(x = x, ymin = ymin, ymax =ymax, group = y_position, fill = fill),
              inherit.aes = FALSE,
              show.legend = FALSE) +
  
  geom_line(color = "#888888", size = 1, show.legend = FALSE) +
  
  geom_point(size = 4, 
             #turns on and off point legend
             show.legend = TRUE) + 
  
  geom_text(aes(label = glue("{percent}%"), 
                x = bump), 
            color = "#686868",
            size = 4,
            show.legend = FALSE,
            family = "lato") +
  scale_color_manual(name = "If a vaccine for COVID-19 were\navailable, I would get it", 
                     breaks = c("august", "october"),
                     values = c("#888888","#59bd74"),
                     labels = c("<span style = 'color:#888888'>August '20</span>",
                                "<span style = 'color:#59bd74'>October '20</span>" 
                     ),
                     #make the dots in legend bigger than dots in graph
                     guide = guide_legend(override.aes = list(size=5))) +
  #Modify the color of the stripes in the background becuase of geom_ribbon
  scale_fill_manual(name = NULL,
                    breaks = c("a","b"),
                    values = c("#f8f8f8","#ffffff"),
                    labels = c("a","b")) +
  scale_x_continuous(limits = c(50, 100),
                     breaks = NULL,
                     labels = NULL,
                     expand = c(0,0)) +
  #building tick marks on y-axis
  scale_y_continuous(breaks = data$y_position,
                     labels = data$country, 
                     expand = c(0,0),
                     limits = c(0.5,16.5)) +
  #use CSS to change colors of letters in a word
  labs(x = "",
       y = NULL, 
       title = "COVID-19 Vaccination Intent is Decreasing Globally",
       # subtitle = "If a vaccine for COVID-19 were available, I would get it",
       caption = "<i>Base: 18,526 online aduts aged 16-74 across 15 countries</i><br> Source: Ipsos") +
  theme_classic() +
  #family of fonts: sans, serif, mono, symbol (doesn't work without matched symbols)
  theme(
    text = element_text(family = "lato"),
    #title
    plot.title.position = "plot",
    plot.title = element_text(face = "bold", 
                              #add space above and below the title
                              margin = margin(b = 30, t =15),
                              color = "#333333",
                              family = "patua-one",
                              size = 25,
                              hjust = 0),
    plot.subtitle = element_text(margin = margin(b = 15, t =0),
                                 color = "#333333",
                                 family = "lato",
                                 size = 15,
                                 hjust = 0),
    #element markdown is needed to use CSS in the caption
    plot.caption = element_markdown(hjust = 0, color = "darkgray",
                                    #Adjust the caption up by using negative number
                                    margin = margin(t = 10,b=10)),
    plot.caption.position = "plot",
    #change the chart background
    plot.background = element_rect(fill = "#ffffff"),
    panel.background = element_blank(),
    plot.margin = margin(l=10, r=20), 
    panel.grid = element_blank(),
    #change the x axis tick marks
    axis.ticks = element_blank(),
    #set color of tick marks (next to country transparent)
    axis.text.y = element_text(size = 12.5),
    #need this line so that you can use CSS for the x axis title
    axis.title.x = element_markdown(size = 25, 
                                    family = "lato",
                                    face = "bold"),
    axis.line = element_blank(),
    legend.position = c(0.08,1.0),
    legend.direction = "horizontal",
    legend.title = element_text(size = 12,
                                lineheight = 1.2,
                                margin = margin(t=-5,b=10, r= 25)),
    legend.justification = "left",
    #remove square behind color dots
    legend.key = element_blank(),
    #Makes the distance smaller between the colored dot and the legend words
    legend.key.width = unit(3, "pt"),
    legend.background = element_blank(),
    #provide spacing between the legend marks
    legend.text = element_markdown(margin = margin(t=3,r=15),
                                   size = 12.5),
    legend.margin = margin(l=0)
  ) +
  guides(fill = "none") +
  coord_cartesian(clip = "off")

