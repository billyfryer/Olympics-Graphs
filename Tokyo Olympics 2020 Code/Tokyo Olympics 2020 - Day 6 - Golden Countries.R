# Olympics Data Viz Project
# Day 6
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Number of Countries with at least 1 medal per over time

# Libraries
library(tidyverse)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

winning_countries <- athlete_events %>% 
  # Summer
  filter(Season == "Summer") %>% 
  # Gold Medal Countries
  filter(Medal == "Gold") %>% 
  # Only Country and Year
  select(NOC, Year) %>% 
  # Distinct Combos
  distinct() %>% 
  # Group by Year
  group_by(Year) %>%
  # Count Number of Countries 
  summarize(WinningCountries = n())

total_countries <- athlete_events %>% 
  # Summer
  filter(Season == "Summer") %>%
  # Only Country and Year
  select(NOC, Year) %>% 
  # Distinct Combos
  distinct() %>% 
  # Group by Year
  group_by(Year) %>% 
  # Count Number of Countries 
  summarize(TotalCountries = n())

# Merge Together
countries <- merge(total_countries, 
                   winning_countries, 
                   by = "Year") %>% 
  # Percent per year
  mutate(Percent = round(100 * WinningCountries / TotalCountries, 1)) %>% 
  # Since 1984
  filter(Year >= 1984)

# Convert Year to a Factor
countries$Year <- factor(countries$Year)

# Plotting!
ggplot(countries, aes(x = Year)) +
  # Black Bar
  geom_bar(aes(y = TotalCountries), 
           stat = "identity", 
           fill = "#666666") + # Dark Grey
  # Golden Bar
  geom_bar(aes(y = WinningCountries),
           stat = "identity",
           fill = "#D6AF36") + # Gold
  # Label for Gold Bar
  geom_text(aes(y = WinningCountries - 10,
                label = WinningCountries),
            color = "#FFFFFF") + # White
  # Label Percentage
  geom_text(aes(y = WinningCountries + 15,
                 label = paste0(Percent, "%")),
            color = "#D6AF36") + # Gold
  ylim(0,210) +
  # Title and captions
  labs(title = "Spreading the Gold",
       subtitle = "Percent of Competing Countries Winning Gold\n at the Summer Games Since 1984",
       caption = "Viz by Billy Fryer | Data From Kaggle",
       y = "Number of Countries") +
  theme( # Title, Subtitle, and Caption
        plot.title = element_text(hjust = 0.5, 
                                  color = "white",
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, 
                                     color = "white"),
        plot.caption = element_text(hjust = 0.5, 
                                    color = "white"),
        # Black Plot Background with White Axis Stuff
        plot.background = element_rect(fill = "black"),
        panel.background = element_rect(fill = "black"),
        # Making the Axises White
        axis.title = element_text(hjust = 0.5, 
                                  color = "white"),
        axis.ticks = element_line(color = "white"),
        axis.text = element_text(color = "white")
        ) +
  # Make Bars Horizontal
  coord_flip()


# ggsave("Day 6- Golden Countries.png",
#        width = 5,
#        height = 4)
