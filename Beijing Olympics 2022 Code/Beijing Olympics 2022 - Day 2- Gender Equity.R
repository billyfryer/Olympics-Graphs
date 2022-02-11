# Olympics Data Viz Project
# Day 2
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Number of Countries with at least 1 medal per over time

# Libraries
library(tidyverse)
library(ggthemes)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

gender_years <- athlete_events %>% 
  # Filter to Winter Games
  filter(Season == "Winter") %>% 
  # Unique ID and Year combos
  # Sex included to make sure it doesn't get dropped
  # The rest of the columns are dropped automatically
  distinct(ID, Year, Sex) %>% 
  # Group By Year
  group_by(Year) %>% 
  # Indicators for Male and Female
  summarize(Male = sum(ifelse(Sex == "M", 1, 0)),
            Female = sum(ifelse(Sex == "F",1, 0))
  ) %>% 
  # Ungroup
  ungroup() %>% 
  # Different from this summer, I want to make it a proportional graph
  mutate(Total = Male + Female,
         Male_Prop = Male / Total,
         Female_Prop = Female / Total) %>%
  # Get rid of raw data values
  select(-c(Male, Female, Total)) %>% 
  # Rename Proportion Columns
  rename("Male" = "Male_Prop",
         "Female" = "Female_Prop") %>% 
  # Make Data Long
  pivot_longer(cols = c(Male, Female), 
               names_to = "Sex",
               values_to = "Proportion")

# Year as a factor variable
gender_years$Year <- factor(gender_years$Year)

### Plotting!
ggplot(gender_years, aes(x = Year,
                         y = Proportion,
                         fill = Sex,
                         group = Sex)) +
  # Area Graph is like a stacked bar chart but continuous
  # Although this data isn't 100% Continuous, I felt it still worked.
  geom_area() + 
  # Wall Street Journal Theme from ggthemes
  theme_wsj() +
  theme( 
    # Rotate X axis text
    axis.text.x = element_text(angle = 270, 
                               vjust = 0.5,
                               hjust = 0),
    # Simple Y axis Title Changes
    axis.title.y = element_text(vjust = 5,
                                size = 13),
    # Center Plot Title and Change Font Size
    plot.title = element_text(hjust = 0.5,
                              size = 16),
    # Center Plot Caption and Change Font Size
    plot.caption = element_text(hjust = 0.5,
                              size = 10),
    # Legend Work
    legend.text.align = 0.5,
    legend.title = element_text(size = 12)) +
  # Set the Fill
  scale_fill_manual(values = c("Male" = "lightblue",
                               "Female" = "lightpink")) +
  # Format Y axis as Percents
  scale_y_continuous(labels = scales::percent) +
  # Labels
  labs(title = "History of Athlete Sex Disparity\nin the Winter Olympics 1924-2014",
       y = "Percent of Athletes",
       caption = "Visualization by Billy Fryer ~ Data from Kaggle")

# ggsave("Day 2- Sex Disparity.png",
#       width = 6,
#       height = 4)
