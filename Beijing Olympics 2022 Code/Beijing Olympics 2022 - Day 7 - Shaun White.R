# Day 7
library(readxl)
library(tidyverse)
library(ggplot2)

# Read in xslx file
data <- read_xlsx("Data Sets/Shaun White Scores.xlsx") 

data <- data %>% 
  # Shaun either won Gold or no medal (which we want to color lightblue)
  # This isn't the best coding practice, 
  # but I was in a rush and knew it would work
  mutate(Medal = case_when(is.na(Medal) ~ "lightblue",
                           TRUE ~ tolower(Medal)))

# Plotting!!!
ggplot(data, aes(x = Year,
                 y = Score,
                 fill = Medal)) +
  # geom_col() == geom_bar(stat = "identity")
  geom_col() +
  # Labels for scores, white background
  geom_label(aes(label = Score),
             fill = "white") +
  # Change Fill to Medal Color
  scale_fill_identity() +
  # Set Breaks for x-axis to be Olympic Years
  scale_x_continuous(breaks = seq(2006,2022, 4)) +
  # This Zooms in On Graph to y - 80 to 100
  coord_cartesian(ylim=c(80,100)) +
  # Labels
  labs(title = "Thank You Shaun White",
       subtitle = "Olympic Finals Score in the Halfpipe\n(2006 and 2010 Scaled to 100 Points)",
       caption = "Data Visualization by Billy Fryer (@_b4billy_)") +
  # Classic Theme
  theme_classic() +
  # Center Title, Subtitle and Caption
  # Bold Title
  theme(
    plot.title = element_text(hjust = 0.5,
                              face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5))

# ggsave("Day 7 - Shaun White.png",
#       width = 6,
#       height = 5.5,
#       units = "in")
