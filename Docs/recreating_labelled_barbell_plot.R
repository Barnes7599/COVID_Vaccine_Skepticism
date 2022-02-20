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

# Ipsos ----

data_ipsos <- COVIDVAX %>% 
    rename(country = X.1, 
           percent_august = 'Total Agree - August 2020',
           percent_october = 'Total Agree - October 2020') %>% 
    mutate(bump_august = case_when(percent_august < percent_october ~ 
                                       percent_august - 2,
                                   percent_august > percent_october ~ 
                                       percent_august + 2,
                                   TRUE ~ NA_real_),
           bump_october = case_when(percent_august < percent_october ~ 
                                        percent_october + 2,
                                    percent_august > percent_october ~
                                        percent_october - 2,
                                    TRUE ~ percent_october + 2),
           y_position = rev(1:nrow(.))) 

main_plot <- data_ipsos %>% 
    select(country, percent_august, percent_october, bump_august, bump_october) %>% 
    pivot_longer(cols = -country, names_to = c(".value", "month"), 
                 names_sep = "_") %>% 
    mutate(country = as_factor(country) %>% fct_rev()) %>% 
    ggplot(aes(percent, country, color = month)) +
    geom_line(color = "#e6e6e6", size = 2, show.legend = FALSE) +
    geom_point(size = 5, show.legend = FALSE) + 
    geom_text(aes(label = glue("{percent}%"), 
              x = bump), 
              size = 4.5,
              show.legend = FALSE) +
    scale_color_manual(name = NULL, 
                       breaks = c("august", "october"),
                       values = c("#727272","#15607a"),
                       labels = c("August", "October")) +
    scale_x_continuous(limits = c(50, 100),
                       breaks = seq(50,100, by = 5),
                       labels = glue("{seq(50, 100, by = 5)}%")) +
    labs(x = NULL,
         y = NULL, 
         title = "If a vaccine for COVID-19 were available, I would get it",
         caption = "<i>Base: 18,526 online aduts aged 16-74 across 15 countries</i><br> Source: Ipsos") +
    theme_classic() +
    theme(plot.title.position = "plot",
          plot.title = element_text(face = "bold", margin = margin(b = 50), size = 25),
          plot.caption = element_markdown(hjust = 0, color = "darkgray"),
          plot.caption.position = "plot",
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(color = "darkgray"),
          panel.grid.major.x = element_line(color = "lightgray", size = 0.1),
          panel.grid.major.y = element_line(color = "lightgray", size = 0.1, linetype = "dotted"))


total <- data_ipsos %>% 
    select(country, percent_august, percent_october, bump_august, bump_october) %>% 
    filter(country == "Total") %>% 
    pivot_longer(cols = -country, names_to = c(".value", "month"), 
                 names_sep = "_") %>% 
    mutate(pretty = if_else(month == "august",
                            "Totally Agree - <br> August 2020",
                            "Totally Agree - <br> October 2020"),
           # 0 = Left justified & 1 = right justified
           align = if_else(month == "august", 0, 1))

main_plot +
    coord_cartesian(clip = "off") +
    # adding labels at the top of the page as a legend
    geom_textbox(data = total, aes(x = percent, 
                                   y = country,
                                   color = month, 
                                   label = pretty, 
                                   hjust = align),
                 size = 5,
                 box.color = NA,
                 box.padding = margin(0,0,0,0),
                 width = NULL, 
                 vjust = -0.5,
                 fill = NA,
                 show.legend = FALSE)





# slope chart----

data <- COVIDVAX %>% 
    rename(country = X.1, 
          august = 'Total Agree - August 2020',
           october = 'Total Agree - October 2020') %>% 
    filter(country != "Total") %>% 
    mutate(country = recode(country, 
                            "South Korea" = "S.Korea",
                            "South Africa" = "S. Africa",
                             "United Kingdom" = "UK",
                            "United States" = "USA")) 
# Geom Segment
data %>%
    ggplot(aes(x="August",
               xend= "October",
               y= august,
               yend=october,
               color = country)) +
    geom_segment()

# Geom Line


country_labels <- data %>% 
    mutate(nrow = 1:n(),
           month = if_else(nrow %% 2 == 0,
                           "August '20", "October '20"),
           percent = if_else(nrow %% 2 == 0,
                             august,october),
           nudge_x =  if_else(nrow %% 2 == 0,
                              -0.05,0.05),
           change = case_when(august < october ~ "increasing",
                              august > october ~ "decreasing",
                              TRUE ~ "stable")
           ) %>% 
    select(country, month, percent, nudge_x, change)




data %>% 
   mutate(country = fct_reorder(country,-october),
          change = case_when(august < october ~ "increasing",
                             august > october ~ "decreasing",
                             TRUE ~ "stable")) %>% 
    
   pivot_longer(cols = c("august","october"), 
                 names_to = "month", 
                 values_to = "percent") %>% 
    mutate(month = factor(month, 
                          levels = c("august", "october"),
                          labels = c("August '20","October '20"))) %>%

    ggplot(aes(x = month, 
               y = percent, 
               group = country,
               color = change)) +
    geom_line(show.legend = FALSE, 
              size = .7) +
    
    geom_text_repel(data = country_labels, 
              aes(x = month, 
                  y= percent, 
                  label = country,  
                  color = change),
              size = 5, 
              # inherit.aes = FALSE,
              hjust = "outward",
              # for geom_label_repel
                  # label.padding = unit(1.1, "mm"),
                  # label.size = unit(.5,"mm"),
                  # label.r = unit(.2,"mm"),
              family = "lato",
              min.segment.length = 0.5,
              nudge_x = country_labels$nudge_x,
              # nudge_y = country_labels$nudge, 
              show.legend = FALSE) +
    scale_y_continuous(breaks = seq(50, 100, 10),
                       lim = c(50,100)) +
    scale_color_manual(values = c("#9e1201", "#b3b3b3", "#b3b3b3")) +
    labs(
         title = "COVID-19 Vaccination Intent is <span style='color:#B51300'>Decreasing</span> Globally",
         subtitle = "From August 2020 to October 2020",
         caption = "<i>Base: 18,526 online aduts aged 16-74 across 15 countries</i><br>Source: Ipsos",
         x = NULL,
         y = "Percent willing to receive vaccine",
         tag = NULL,
         color = NULL
    ) +
    theme(
        text = element_text(family = "lato"),
        plot.title = element_textbox_simple(family = "patua-one",
                                      size = 33,
                                      lineheight = 1,
                                      margin = margin(b=10)), 
        plot.caption =  element_markdown(lineheight = 1.3,
                                         margin = margin(t=10),
                                         hjust = 0,
                                         color = "#666666",
                                         family = "lato"),
        plot.subtitle = element_text(family = "lato",
                                     size = 18),
        axis.title.y = element_text(color = "#888888",
                                    size = 12),
        axis.text.y = element_text(color = "#888888", 
                                   size = 12),
        axis.text.x = element_text(color = "#888888",
                                   size = 12),
        axis.line = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
    ) +
    theme(
        text = element_text(family = "lato"),
        plot.title = element_textbox_simple(family = "patua-one",
                                            size = 33,
                                            lineheight = 1,
                                            margin = margin(b=10)), 
        plot.caption =  element_markdown(lineheight = 1.3,
                                         margin = margin(t=10),
                                         hjust = 0,
                                         color = "#666666",
                                         family = "lato"),
        plot.subtitle = element_text(family = "lato",
                                     size = 18),
        axis.title.y = element_text(color = "#888888",
                                    size = 12),
        axis.text.y = element_text(color = "#888888", 
                                   size = 12),
        axis.text.x = element_text(color = "#888888",
                                   size = 12),
        axis.line = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
    )




# Custom Theme darkmode()----



p <- data %>% 
    mutate(country = fct_reorder(country,-october),
           change = case_when(august < october ~ "increasing",
                              august > october ~ "decreasing",
                              TRUE ~ "stable")) %>% 
    
    pivot_longer(cols = c("august","october"), 
                 names_to = "month", 
                 values_to = "percent") %>% 
    mutate(month = factor(month, 
                          levels = c("august", "october"),
                          labels = c("August '20","October '20"))) %>%
    
    ggplot(aes(x = month, 
               y = percent, 
               group = country,
               color = change)) +
    geom_line(show.legend = FALSE, 
              size = .7) +
    
    geom_text_repel(data = country_labels, 
                    aes(x = month, 
                        y= percent, 
                        label = country,  
                        color = change),
                    size = 5, 
                    # inherit.aes = FALSE,
                    hjust = "outward",
                    # for geom_label_repel
                    # label.padding = unit(1.1, "mm"),
                    # label.size = unit(.5,"mm"),
                    # label.r = unit(.2,"mm"),
                    family = "lato",
                    min.segment.length = 0.5,
                    nudge_x = country_labels$nudge_x,
                    # nudge_y = country_labels$nudge, 
                    show.legend = FALSE) +
    scale_y_continuous(breaks = seq(50, 100, 10),
                       lim = c(50,100)) +
    scale_color_manual(values = c("#9e1201", "#b3b3b3", "#b3b3b3")) +
    labs(
        title = "COVID-19 Vaccination Intent is <span style='color:#B51300'>Decreasing</span> Globally",
        subtitle = "From August 2020 to October 2020",
        caption = "<i>Base: 18,526 online aduts aged 16-74 across 15 countries</i><br>Source: Ipsos",
        x = NULL,
        y = "Percent willing to receive vaccine",
        tag = NULL,
        color = NULL
    ) 


theme_darkmode <- function(bg = "#212121", 
                           primary = "#ffffff", 
                           secondary = "##B51300"){
    
    theme_gray() %+replace%
        
    theme(
        text = element_text(family = "lato"),
        plot.title = element_textbox_simple(family = "patua-one",
                                            size = 33,
                                            lineheight = 1,
                                            margin = margin(b=10),
                                            color = primary
                                            ),
        plot.caption =  element_markdown(lineheight = 1.3,
                                         margin = margin(t=10),
                                         hjust = 0,
                                         color = primary,
                                         family = "lato"),
        plot.subtitle = element_text(family = "lato",
                                     size = 18,
                                     color = primary,
                                     hjust = 0),
        axis.title.y = element_text(color = primary,
                                    size = 12,
                                    angle = 90,
                                    margin = margin(r=10)),
        axis.text.y = element_text(color = primary,
                                   size = 12),
        axis.text.x = element_text(color = primary,
                                   size = 12),
        axis.line.y = element_blank(),
        axis.ticks = element_blank(),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = bg,
                                        color = NA),
        plot.background = element_rect(fill = bg,
                                       color = NA)
        
    )
}

    
p + 
    theme_darkmode()
