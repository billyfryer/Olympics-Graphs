# Olympics Data Viz Project
# Day 4
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)
library(ggthemes)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data
noc_regions <- read.csv("Data Sets/noc_regions.csv") # Olympic Countries Data

# Replace "Athina" with "Athens" for City
athlete_events$City[athlete_events$City == "Athina"] <- "Athens"

### Manipulate Data
softball_players <- athlete_events %>%
  # Only Softball
  filter(Sport == "Softball") %>%
  # Make Games Variable for Display:
  # City
  # Year
  mutate(Games = paste(City, Year, sep = "\n")) %>%
  # Select Only a Few Columns
  select(Age, Year, Games, Name, NOC, Medal) %>%
  # Only USA and Japan
  filter(NOC %in% c("USA", "JPN"))

# Create No Medal Category
softball_players$Medal[is.na(softball_players$Medal)] <- "No Medal"

# Make Games a Factor Variable
softball_players$Games <- factor(softball_players$Games)

# Join Full Country Name
softball_players <- left_join(softball_players, noc_regions, by = "NOC") %>% 
  # Rename region to Country
  rename("Country" = "region") %>% 
  # Get rid of notes column
  select(-notes)

### Plotting!
ggplot(softball_players, aes(x = reorder(Games, Year),
                             y = Age,
                             fill = Medal)) +
  theme_dark() +
  geom_violin() +
  labs(title = "Ages of US and Japan Softball Players\nDuring the Olympics",
       x = "",
       fill = "Medal Won",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data From Kaggle") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  # Set Break Points For Age on Y Axis
  scale_y_continuous(breaks = c(15,20,25,30,35,40)) +
  scale_fill_manual(values = c("Gold" = "#D6AF36",
                               "Silver" = "#A7A7AD",
                               "Bronze" = "#824A0D",
                               "No Medal" = "#FFFFFF"
                               )) +
  # Split By Country and have them on top of one another
  facet_wrap(~Country, ncol = 1)

# ggsave("Day 4- US v Japan Softball.png",
#        width = 5.3,
#        height = 4)
