library(tidyverse)
library(readxl)
library(glue)
library(tidyquant)
library(ggtext)
library(showtext)
library(ggrepel)
library(glue)

#add in goggle fonts
font_add_google(family = "josefin-slab","Josefin Slab")
font_add_google(family = "josefin-sans", "Josefin Sans")
font_add_google(family = "patua-one", "Patua One")
font_add_google(family = "lato", "Lato")
font_add_google(family = "montserrat", "Montserrat")

#function used to tell the code below use the above fonts
showtext_auto()

COVIDVAX <- read_excel("COVIDVAX.xlsx")

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



# Chartr ----


data_chartr <- COVIDVAX %>% 
    rename(country = X.1, 
           percent_august = 'Total Agree - August 2020',
           percent_october = 'Total Agree - October 2020') %>% 
    #adjusts percent data labels
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
           y_position = rev(1:nrow(.))) %>% 
    mutate(country = recode(country, 
                            "South Korea" = "S.Korea",
                            "South Africa" = "S. Africa",
                            "United Kingdom" = "UK",
                            "United States" = "USA"
    )) %>% 
    filter(country != "Total")

striped_data <- data_chartr %>% 
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

arrows_data <- data_chartr %>% 
    filter(abs(percent_august - percent_october) > 1) %>% 
    mutate(midpoint = (percent_august + 2*percent_october)/3) %>% 
    select(country, y_position, percent_august, midpoint) %>% 
    pivot_longer(c(percent_august, midpoint), names_to = "type", values_to = "x")
    

data_chartr %>% 
    pivot_longer(cols = -c(country, y_position), 
                 names_to = c(".value", "month"), 
                 names_sep = "_") %>% 
    drop_na() %>% 
#   mutate(country = as_factor(country) %>% fct_rev()) %>% 
    ggplot(aes(percent, y_position, color = month, group = y_position)) +
    geom_ribbon(data = striped_data,
                aes(x = x, ymin = ymin, ymax =ymax, group = y_position, fill = fill),
                inherit.aes = FALSE,
                show.legend = FALSE) +
    
    geom_line(color = "#153744", size = 0.75, show.legend = FALSE) +
    #adding arrow segments
    geom_path(data = arrows_data, aes(x=x, y=y_position, group = y_position),
              color = "#153744",
              size = 0.75,
              arrow = arrow(angle = 30, 
                            length = unit(0.1, "inches"), 
                            ends = "last", 
                            type = "closed"),
              show.legend = FALSE,
              inherit.aes = FALSE) +
    
    geom_point(size = 3, show.legend = TRUE) + 
    
    geom_text(aes(label = glue("{percent}%"), 
                  x = bump), 
              color = "#686868",
              size = 4.5,
              show.legend = FALSE,
              family = "josefin-sans") +
    scale_color_manual(name = "If a vaccine for COVID-19 were\navailable, I totally agree I would get it...", 
                       breaks = c("october", "august"),
                       values = c("#59ac74", "#153744"),
                       labels = c("<span style = 'color:#59ac74'>October '20</span>", 
                                  "<span style = 'color:#153744'>August '20</span>"),
                       #make the dots in legend bigger than dots in graph
                       guide = guide_legend(override.aes = list(size=5))) +
    #Modify the color of the stripes in the background
    scale_fill_manual(name = NULL,
                      breaks = c("a","b","c"),
                      values = c("#dfeaf9","#edf4f7","#f3fafe"),
                      labels = c("a","b","c")) +
    scale_x_continuous(limits = c(50, 100),
                       breaks = seq(50,100, by = 5),
                       labels = glue("{seq(50, 100, by = 5)}%"),
                       expand = c(0,0)) +
    #building tick marks on y-axis
    scale_y_continuous(breaks = c(data_chartr$y_position, 0.5, data_chartr$y_position + .5,
                                  length(data_chartr$y_position) + 1.5),
                       labels = c(data_chartr$country, rep("", length(data_chartr$y_position)+2)),
                       expand = c(0,0),
                       limits = c(0.5,16.5)) +
    #use CSS to change colors of letters in a word
    labs(x = "<span style = 'color:#4da6be;'>chart</span><span style ='color:#e9b388;'>r</span>",
         y = NULL, 
         title = "Vaccine Skepticism by Country",
         caption = "Source: Ipsos") +
    theme_classic() +
    #family of fonts: sans, serif, mono, symbol (doesn't work without matched symbols)
    theme(
          text = element_text(family = "josefin-sans"),
          #title
          plot.title.position = "plot",
          plot.title = element_text(face = "bold", 
                                    #add space above and below the title
                                    margin = margin(b = 35, t =15),
                                    color = "#2e737b",
                                    family = "josefin-slab",
                                    size = 30,
                                    hjust = 0),
          #element markdown is needed to use CSS in the caption
          plot.caption = element_markdown(hjust = 0, color = "darkgray",
                                          #Adjust the caption up by using negative number
                                          margin = margin(t=-15)),
          plot.caption.position = "plot",
          #change the chart background
          plot.background = element_rect(fill = "#f3fafe"),
          panel.background = element_blank(),
          plot.margin = margin(l=10, r=20), 
          panel.grid = element_blank(),
          #change the x axis tick marks
          axis.ticks.x = element_blank(),
          #set color of tick marks (next to country transparent)
          axis.ticks.y = element_line(color = c(rep(NA, nrow(data_chartr)),
                                                rep("darkgray", nrow(data_chartr) +2)),
                                      size = 0.2), 
          axis.text.x = element_text(color = "#686868", 
                                     size = 12.5),
          axis.text.y = element_text(size = 12.5),
          #need this line so that you can use CSS for the x axis title
          axis.title.x = element_markdown(size = 25, 
                                          family = "josefin-slab",
                                          face = "bold"),
          axis.line = element_line(color = "darkgray",
                                   size = 0.2),
          legend.position = c(0,1.0),
          legend.direction = "horizontal",
          legend.title = element_text(size = 12,
                                      lineheight = 1.2,
                                      margin = margin(b=5)),
          legend.justification = "left",
          #remove square behind color dots
          legend.key = element_blank(),
          #Makes the distance smaller between the colored dot and the legend words
          legend.key.width = unit(3, "pt"),
          legend.background = element_blank(),
          #provide spacing between the legend marks
          legend.text = element_markdown(margin = margin(l=5,r=10),
                                         size = 12.5)
    ) +
    guides(fill = "none") +
    coord_cartesian(clip = "off")




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
    scale_y_continuous(breaks = data_chartr$y_position,
                       labels = data_chartr$country, 
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
                                     color = primary),
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

# Different Branch

# Set Colors 
base <- "#FFFFFF"    
primary <- "#AAAAAA"
secondary <- "#000000"
highlight <- "#B51300"

# Set Text
title_name <- "patua-one"
body <-  "montserrat"

title <- glue("Between August and October of 2020, <span style='color:#AAAAAA'>people's intention</span> to receive the COVID-19 vaccine <span style='color:#B51300'>{data_highlight_title$direction} by {data_highlight_title$diff}% in the {data_highlight_title$country}</span>")


data_highlight <- COVIDVAX %>% 
    rename(country = X.1, 
           august = 'Total Agree - August 2020',
           october = 'Total Agree - October 2020') %>% 
    filter(country != "Total") %>% 
    mutate(country = recode(country, 
                            "South Korea" = "S.Korea",
                            "South Africa" = "S. Africa",
                            "United Kingdom" = "UK",
                            "United States" = "USA")) %>% 
    pivot_longer(-country, names_to = "month", values_to = "percent") %>% 
    mutate(country_highlight = (country == "USA"),
           country = fct_reorder(country, country_highlight)) 

data_highlight_title <- data_highlight %>% 
    group_by(country) %>% 
    summarise(diff = diff(percent),
              direction = case_when(diff < 0 ~ "dropped",
                                    diff >0 ~ "increased",
                                    TRUE ~ "remained flat"),
              diff = abs(diff)) %>% 
    filter(country == "USA")

data_highlight %>%  
    ggplot(aes(month, percent, group = country, 
               color = country_highlight, 
               size = country_highlight)) +
    geom_line(show.legend = FALSE,
              lineend = "round") +
    geom_point(show.legend = FALSE, 
               size = 2.5) +
    labs(title = title,
         x = NULL, 
         y = " Percent willing to receive vaccine",
         caption = "<i>Base: 18,526 online aduts aged 16-74 across 15 countries</i><br>Source: Ipsos") +
    scale_x_discrete(breaks = c("august", "october"),
                     labels = c("August '20", "October '20"),
                     expand = c(0.15,0.15)) +
    scale_y_continuous(limits = c(50,100),
                       breaks = seq(50,100, by = 10),
                       minor_breaks = NULL,
                       expand = c(0.03, 0.03)) +
    scale_color_manual(breaks = c(F,T),
                       values = c(primary, highlight)) + 
    scale_size_manual(breaks = c(F,T),
                      values = c(0.5, 1.5)) +
    theme(text = element_text(family = body,
                              color = primary),
          plot.title.position = "plot",
          plot.title = element_textbox_simple(family = title_name,
                                              face = "bold",
                                              color = secondary,
                                              size = 20,
                                              margin = margin(t=10, b =10)),
          plot.background = element_rect(fill = base),
          panel.background = element_rect(fill = base),
          plot.caption.position = "plot",
          plot.caption = element_markdown(hjust = 0,
                                          color = primary,
                                          margin = margin(t=10)),
          axis.text = element_text(color = primary,
                                   size = 12),
          axis.title = element_text(size = 14),
          axis.title.y = element_text(margin = margin(r=5)),
          axis.ticks = element_blank())

