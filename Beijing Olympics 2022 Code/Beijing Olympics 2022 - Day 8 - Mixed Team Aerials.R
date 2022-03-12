# Day 8
library(tidyverse)
library(ggplot2)
library(ggimage)
library(png)

# Read in Data
scores <- read_csv("Data Sets/Freestyle Skiing Mixed Team Aerials Finals Scores.csv")

# Add on a Image Column
final_data <- scores %>% 
  mutate(Image = paste0("Flags and Icons/Flags/", Participant, ".png"))

# Beijing 2022 Logo
logo <- png::readPNG("Flags and Icons/Logos/Beijing 2022 Logo.png")

# Plotting!
ggplot(final_data, aes(x = reorder(Participant, desc(Result)),
                   y = Result,
                   fill = Participant)) +
  # Bar Chart with black outlines
  geom_col(color = "black",
           width = 0.825) +
  # This Zooms in On Graph to y - 80 to 100
  coord_cartesian(ylim=c(250,350)) +
  # Changes fill of the color
  scale_fill_manual(values = c("Canada" = "#000000",
                               "China" = "#FFFF00",
                               "Switzerland" = "#FFFFFF",
                               "United States" = "#0A3161")) +
  # Labels
  labs(x = "",
       y = "Combined Team Score",
       title = "Freestyle Skiing Mixed Team Aerials Finals Scores",
       caption = "Visualization by Billy Fryer (@_b4billy_) ~ Data from Olympics.com") +
  # Classic Theme
  theme_classic() +
  # No Legend and Centered title, subtitle, cpaation
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  # Attach Flags to each Bar
  geom_image(aes(image = Image),
             size = 0.2,
             by = "width") +
  # Attach Rings Photo
  annotation_raster(logo, 
                    # X Locations
                    xmin = "Canada", xmax = "Switzerland",
                    # Y Locations
                    ymin = 310, ymax = 350)

# ggsave("Outputs/Beijing 2022 Output/Day 8 - Freestyle Skiing Mixed Team Ariels Finals.png",
#        width = 6.75,
#        height = 5.25)
