# Olympics Data Viz Project
# Day 2
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

### Manipulate Data
volleyball_heights_long <- athlete_events %>%
  # Only Volleyball
  filter(Sport == "Volleyball") %>%
  # Only USA
  filter(NOC == "USA") %>% 
  # Group By Sex and Year
  group_by(Sex, Year) %>% 
  # Get Mean Height (cm)
  summarize(MeanHeight = mean(Height, na.rm = TRUE)) %>% 
  # Ungroup
  ungroup() %>% 
  # Arrange by Year
  arrange(Year) %>% 
  # Convert to Inches
  mutate(MeanHeight = MeanHeight / 2.54)

### Plotting
ggplot(volleyball_heights_long, 
       aes(x = Year, 
           y = MeanHeight)) +
  # Make labels for x axis only Olympic Years
  scale_x_continuous(breaks=c(1968, 1976, 1984, 1992,
                              2000, 2008, 2016)) +
  # Add a Smoothing Curve
  geom_smooth(se = FALSE, aes(color = Sex)) +
  # Black Points for exact Values
  geom_point(color = "#000000") +
  # Color
  scale_color_manual(values = c("M" = "lightblue",
                                "F" = "pink")) +
  # 6 Feet Tall Line
  geom_hline(yintercept = 72, color = "black") +
  geom_text(x = 1972.5, y = 72.4, label = "6'") +
  # 6 Feet 6 Line
  geom_hline(yintercept = 78, color = "black") +
  geom_text(x = 1972.5, y = 78.4, label = "6' 6\"") +
  # Labels
  labs(title = "Team USA Volleyball Average Height",
       subtitle = "USA Volleyball Did Not Qualify/Compete in 1972, 1976, and 1980",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data from Kaggle",
       x = "Year",
       y = "Height (Inches)") +
  # Change Theme
  theme_classic() +
  # Center Labels
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

# ggsave("Day 2- Team USA Volleyball.png",
#        width = 5.75,
#        height = 4.75)
