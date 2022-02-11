# Beijing Olympics Day 3 Graph
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggimage)
library(ggpubr)
library(png)
#' Data typed manually in from https://www.eurosport.com/olympics/athletes/country/all

# Read xlsx data
athlete_counts <- read_xlsx("Data Sets/Athletes per Country Beijing 2022.xlsx") %>% 
  # Only Countries with 100 or More Athletes
  filter(Athletes >= 100) %>% 
  # Mutate Flags Column
  mutate(Flag = paste0("Flags and Icons/Flags/",Country,".png"))

# Read In Olympic Rings logo
rings <- png::readPNG("Flags and Icons/Logos/Olympic Rings.png")

### Plotting!
ggplot(athlete_counts, aes(x = reorder(Country, desc(Athletes)),
                           y = Athletes)) +
  # Bar Graph
  geom_bar(stat = "identity",
           fill = "lightblue",
           # This makes it more see through
           alpha = 0.5) +
  # Flags
  geom_image(aes(image = Flag,
                 y = Athletes),
             size = 0.05,
             by = "width") +
  # Labels
  labs(title = "Largest Delegations at Beijing 2022",
       subtitle = "Countries with over 100 Athletes at the Winter Olympics",
       caption = "Visualization by Billy Fryer ~ Data from Eurosport",
       x = "") +
  # Minimal Theme
  theme_minimal() +
  # Centering Titles
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "black"),
    panel.grid.minor = element_blank()
    ) +
  # Attach Rings Photo
  annotation_raster(rings, 
                    # X Locations
                    xmin = "Slovakia", xmax = "Norway",
                    # Y Locations
                    ymin = 300, ymax = 400)

# ggsave("Day 3 - Largest Delegations at Beijing 2022.png",
#       width = 11,
#       height = 6)
