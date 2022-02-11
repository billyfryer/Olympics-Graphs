# Olympics Data Viz Project
# Day 14
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)
library(devtools)
#install_github("EmilHvitfeldt/emoji")
library(emoji)
library(png)

# devtools::install_github("EmilHvitfeldt/emoji")
# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

avg_marathoners <- athlete_events %>% 
  # Group By Year
  group_by(Year) %>% 
  # Mean Height Calculation
  mutate(meanHeight = mean(Height, na.rm = TRUE)) %>% 
  # Ungroup
  ungroup() %>% 
  # Only select Year and meanHeight Columns
  select(Year, meanHeight) %>% 
  # Only Unique Rows
  unique() %>% 
  # Convert to Inches
  mutate(meanHeight = meanHeight / 2.54)

# Summarize athlete_events to get medal counts by athlete
marathoners <- athlete_events %>%
  # Only the Women's Marathon
  filter(grepl("Women's Marathon", Event)) %>% 
  # Only Medal Winners
  filter(!is.na(Medal)) %>% 
  # Medal Emoji
  mutate(emoji = emoji::medal(Medal)) %>% 
  # Join avg_marathoner's height
  left_join(avg_marathoners, by = "Year") %>% 
  # Convert Height to Inches
  mutate(Height = Height / 2.54)

# Olympic Rings Photo
rings <- readPNG("Flags and Icons/Logos/Olympic Rings.png")

### Plotting!
ggplot(marathoners, aes(x = Year,
                        y = Height,
                        color = Medal
                        )) +
  # Attach Rings Photo
  annotation_raster(rings, 
                    # X Locations
                    xmin = 2002, xmax = 2014,
                    # Y Locations
                    ymin = 66.5, ymax = 68.5) +
  # Medal Emojis
  geom_text(aes(label = emoji), size = 8) +
  # Specific Point
  geom_point() +
  # Mean Height Point in Red
  geom_point(aes(y = meanHeight), color = "red", size = 3) +
  # Color Selecting for Medals
  scale_color_manual(values = c("Gold" = "#D6AF36",
                               "Silver" = "#A7A7AD",
                               "Bronze" = "#824A0D")) +
  # Labels and Titles
  labs(title = "Olympic Women's Marathoners by Height",
       subtitle = "Red Dot Represents Mean Height of Marathoners that Year",
       caption = "Viz by Billy Fryer (@_b4billy) | Data from Kaggle",
       y = "Height in Inches") +
  theme(# No Legend
        legend.position = "none",
        # Dark Blue Background
        panel.background = element_rect(fill = "darkblue"),
        # Only vertical major grid lines
        panel.grid.major.x = element_line(color = "white",
                                          linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        # Center Title, Subtitle and Caption
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  # Select X values
  scale_x_continuous(breaks = c(1984, 1988, 1992, 1996, 2000, 2004,
                                2008, 2012, 2016))

# ggsave("Day 14- W Marathoners by Height.png",
#        width = 5.5,
#        height = 4)
