# Olympics Data Viz Project
# Day 15
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(devtools)
#install_github("rensa/ggflags")
library(tidyverse)
library(ggplot2)
library(scales)
library(ggrepel)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

# Facet by NOC
# Gold Medals / Number of events competed in

# Get Gold Count for each country by year
countries <- athlete_events %>% 
  # Summer
  filter(Season == "Summer") %>% 
  # Group by Year and NOC
  group_by(Year, NOC) %>%
  # Summarize
  summarize(# Medal Count
            Gold = sum(ifelse(Medal == "Gold", 1, 0), na.rm = TRUE),
            Silver = sum(ifelse(Medal == "Silver", 1, 0), na.rm = TRUE),
            Bronze = sum(ifelse(Medal == "Bronze", 1, 0), na.rm = TRUE),
            TotalMedals = sum(Gold, Silver, Bronze, na.rm = TRUE),
            # Total Athletes
            TotalAthletes = n(),
            # Athletes that didn't win
            LoserAthletes = TotalAthletes - TotalMedals) %>%
  # Unique Rows
  unique() %>% 
  # Ungroup
  ungroup() %>% 
  # Replace NAs with 0s
  replace_na(list(Gold = 0,
                  Silver = 0,
                  Bronze = 0,
                  TotalMedals = 0)) %>% 
  # TotalMedals / NumAthletes
  mutate(PercentWinners = TotalMedals / TotalAthletes) %>% 
  # Filtering to make it more manageable
  filter(TotalAthletes >= 100) %>% 
  # Arrange Rows
  arrange(desc(PercentWinners), desc(TotalAthletes)) %>% 
  # Years Since 1960
  filter(Year >= 1960)

# Plotting!
ggplot(countries, aes(x = Year,
                      y = PercentWinners,
                      group = Year
                      )) +
  # Boxplot
  geom_boxplot(fill = "lightblue",
               outlier.color = "navy") +
  # X limits
  xlim(1958,2018) +
  # label_percent is from scales package 
  scale_y_continuous("Percent of Athletes", 
                     labels = label_percent()) +
  # Text about USSE
  geom_label_repel(data = subset(countries, PercentWinners > .6),
             label = "USSR in the Boycotted\n1980 Moscow Games",
             nudge_x = 20) +
  # Labels and Title
  labs(title = "Percent of Winning Athletes by Country (Min 100 Athletes Sent)",
       subtitle = "Total Number of Medals Won / Total Athletes Sent By Country and Year in Summer Games since 1960",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data From Kaggle") +
  theme(# Plot Title, Subtitle, Caption and Axis Text
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold",
                                  color = "black"),
        plot.subtitle = element_text(hjust = 0.5,
                                     color = "black"),
        plot.caption = element_text(hjust = 0.5,
                                     color = "black"),
        axis.text = element_text(color = "black"),
        # Change Background Color to Light Red
        plot.background = element_rect(fill = "#FF7F7F"))


# ggsave("Day 15- Percent Winners.png",
#        width = 8,
#        height = 4)
