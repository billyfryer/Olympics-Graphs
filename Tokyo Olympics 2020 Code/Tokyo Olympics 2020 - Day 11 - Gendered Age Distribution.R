# Olympics Data Viz Project
# Day 11
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Number of Countries with at least 1 medal per over time

# Libraries
library(tidyverse)
library(ggthemes)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

# Easy Data Manipulation Today
athlete_events2 <- athlete_events %>%
  # Summers Since 2000
  filter(Season == "Summer" & Year >= 2000) %>% 
  # Only Age Sport and Sex
  select(Sport, Age, Sex) %>% 
  # Get rid of ALL NAs
  na.omit() %>% 
  # Group By Sport and Sex
  group_by(Sport, Sex) %>% 
  # Keep Age while making MedianAge Column by Sport and Sex
  summarize(Age = Age,
            MedianAge = median(Age))

# Make Sport a Factor
athlete_events2$Sport <- factor(athlete_events2$Sport)

### Plotting

# Only Males from athlete_events2
ggplot(subset(athlete_events2, Sex == "M"),
    aes(x = reorder(Sport, MedianAge), 
        y = Age)) +
  geom_violin(fill = "blue") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 270),
        plot.title = element_text(size = 15,
                                  hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "",
       y = "",
    title = "Male Summer Olympians Age Distribution Since 2000 By Sport",
    caption = "Viz By Billy Fryer (@_b4billy_) | Data From Kaggle | Inspiration by @StatsInTheWild",
    subtitle = "Yellow Dot Represents the Median Age Per Sport") +
  # Add Yellow Point Where Median Is
  geom_point(aes(y = MedianAge),
             color = "yellow")
# Save Male Plot
# ggsave("Day 12- Male Age Distribution.png", 
#        width = 7.5,
#        height = 4.5,
#        units = "in")

# Same Plot as Above but for Females
ggplot(subset(athlete_events2, Sex == "F"),
       aes(reorder(Sport, MedianAge), 
           y = Age)) +
  geom_violin(fill = "hotpink") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 270),
        plot.title = element_text(size = 15,
                                  hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = "",
       y = "",
       title = "Female Summer Olympians Age Distribution Since 2000 By Sport",
       caption = "Viz By Billy Fryer (@_b4billy_) | Data From Kaggle | Inspiration by @StatsInTheWild",
       subtitle = "Yellow Dot Represents the Median Age Per Sport") +
  geom_point(aes(y = MedianAge),
             color = "yellow")

# Save Female Plot
# ggsave("Day 11- Female Age Distribution.png",
#        width = 7.5,
#        height = 4.5,
#        units = "in")
