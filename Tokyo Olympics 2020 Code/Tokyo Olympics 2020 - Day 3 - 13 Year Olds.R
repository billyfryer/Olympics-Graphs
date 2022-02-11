# Olympics Data Viz Project
# Day 3
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Flags from Google stored on local computer

# Libraries
library(tidyverse)
library(gt)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data
noc_regions <- read.csv("Data Sets/noc_regions.csv") # Olympic Countries Data

### Manipulate Data
youngest_winners <- athlete_events %>%
  # Gold Medals
  filter(Medal == "Gold") %>%
  # Summer Games
  filter(Season == "Summer") %>%
  # Only 13 year olds
  filter(Age == 13) %>% 
  # Arrange Rows by Year, Name then NOC
  arrange(Year, Name, NOC) %>% 
  # Select Columns
  select(Name, Age, NOC, Year, Sport)

# Add 2020 Skateboarder to data frame 
tokyo <- c("Momiji Nishiya", 13, "JPN", 2020, "Skateboarding")
youngest_winners <- rbind(youngest_winners, tokyo)

# Make Year Numeric
youngest_winners$Year <- as.numeric(youngest_winners$Year)

# Join Full Country Name
youngest_winners <- left_join(youngest_winners, noc_regions, by = "NOC") %>% 
  rename("Country" = "region") %>% 
  select(Name, Age, Year, Sport, Country, NOC)

# Join Country Flag
flags <- data.frame(NOC = c("USA", "SUI", "GER", "CHN", "JPN"),
                    Flag = c("Flags and Icons/Flags/USA", 
                             "Flags and Icons/Flags/Switzerland", 
                             "Flags and Icons/Flags/Germany",
                             "Flags and Icons/Flags/China",
                             "Flags and Icons/Flags/Japan"))

# Join flags to youngest_winners
youngest_winners <- left_join(youngest_winners, flags, by = "NOC") %>% 
  select(Name, Age, Year, Country, Flag, Country)

### Table Making
youngest_winners %>% 
  gt() %>%
  # Title and Subtitle
  tab_header(
    title = "When I Was Your Age...",
    subtitle = "Gold Medals Won in the Summer Olympics by 13 Year Olds"
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Data from Kaggle") %>%
  # Tab Options
  tab_options(
    # Change Font size for subtitle
    heading.subtitle.font.size = 12,
    # Center Align Heading
    heading.align = "center",
    # Make Very Top Line Black
    table.border.top.color = "black",
    # Make Line Between Title and Headers Black and Width 3
    column_labels.border.top.color = "black",
    column_labels.border.top.width= px(3),
    # Make Line Between Headers and Data Black and Width 3
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width= px(3)
  ) %>%
  # Convert Flag Column to the Images saved in directory
  text_transform(
    locations = cells_body(Flag),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0(.x, ".png"),
        height = 25
      ))
    })

gtsave("Day 3- 13 Year Olds.png")
