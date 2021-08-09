# Olympics Data Viz Project
# Day 8
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data
noc_regions <- read.csv("Data Sets/noc_regions.csv") # Olympic Countries Data

height_by_country <- athlete_events %>% 
  # Summer
  filter(Season == "Summer") %>% 
  # Only where Height is Availble
  filter(!is.na(Height)) %>% 
  # Group by NOC and Year
  group_by(NOC, Year) %>% 
  # Summary Stuff group_by(NOC, Year) %>% 
  summarise(Tallest = max(Height) / 2.54, # Converted to Inches
            Shortest = min(Height) / 2.54, # Converted to Inches
            Diff = Tallest - Shortest) %>% 
  # Ungroup
  ungroup() %>% 
  # Join Full Country Names
  left_join(noc_regions, by = "NOC") %>% 
  # Get Rid of notes
  select(-notes)

# Plotting!
ggplot(subset(height_by_country, NOC %in% c("USA", "GBR", "AUS", "CAN")),
       aes(x = Year)) +
  # Blue Ribbon
  geom_ribbon(aes(ymax = Tallest,
                  ymin = Shortest), 
            size = 2,
            fill = "lightblue")+
  # Top Red Line
  geom_path(aes(y = Tallest),
             color = "red",
            size = 1.5) +
  # Bottom Red Line
  geom_path(aes(y = Shortest),
             color = "red",
            size = 1.5) +
  # Top Points
  geom_point(aes(y = Tallest),
             color = "white",
             size = 3) +
  # Bottom Points
  geom_point(aes(y = Shortest),
             color = "white",
             size = 3) +
  # Dark Theme
  theme_dark() +
  # Facet by Country
  facet_wrap(~region) +
  labs(title = "Summer Olympians Height Differential Over Time",
       subtitle = "Distance Between the Shortest and Tallest Athletes Per Country",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data From Kaggle",
       x = "Year",
       y  = "Height in Inches"
       ) +
  theme( # Turn Year Vertical
        axis.text.x = element_text(angle = 270, 
                                   vjust = 0.5,
                                   hjust = 0),
        # Centering Titles and Caption
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

# ggsave("Day 9- Height Differential.png",
#        width = 6,
#        height = 4)

###################################################
# Not Separated By Country
###################################################

height_by_country <- athlete_events %>% 
  # Summer
  filter(Season == "Summer") %>% 
  # Where Height is Availible
  filter(!is.na(Height)) %>% 
  # Group By Year (Not NOC)
  group_by(Year) %>% 
  # Summary Stuff
  summarise(Tallest = max(Height) / 2.54, # Converted to Inches
            Shortest = min(Height) / 2.54, # Converted to Inches
            Diff = Tallest - Shortest) %>%
  # Ungroup
  ungroup()

### Same Plot as Before Without Faceting by NOC
ggplot(height_by_country, aes(x = Year)) +
  geom_ribbon(aes(ymax = Tallest,
                  ymin = Shortest), 
              size = 2,
              fill = "lightblue")+
  geom_path(aes(y = Tallest),
            color = "red",
            size = 1.5) +
  geom_path(aes(y = Shortest),
            color = "red",
            size = 1.5) +
  geom_point(aes(y = Tallest),
             color = "white",
             size = 3) +
  geom_point(aes(y = Shortest),
             color = "white",
             size = 3) +
  theme_dark() +
  labs(title = "Summer Olympians Height Differential Over Time",
       subtitle = "Distance Between the Shortest and Tallest Summer Olympic Athletes Worldwide",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data From Kaggle",
       x = "Year",
       y  = "Height in Inches"
  ) +
  theme(axis.text.x = element_text(angle = 270, 
                                   vjust = 0.5,
                                   hjust = 0),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))


# ggsave("Day 8- Overall Height Differential.png",
#        width = 6,
#        height = 4)
