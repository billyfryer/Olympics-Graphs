# Olympics Data Viz Project
# Day 13
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)
library(gt)
library(ggrepel)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data
noc_regions <- read.csv("Data Sets/noc_regions.csv") # Olympic Countries Data


# Athletes that won medals in both the summer and winter Olympics
both_seasons <- athlete_events %>% 
  # Only Medal Winners
  filter(!is.na(Medal)) %>% 
  # Group By ID
  group_by(ID) %>%
  summarize(
    Name = Name,
    NOC = NOC,
    # People in both summer and winter will be "Summer, Winter"
    Season = paste(unique(Season) %>% str_sort(), collapse = ", "),
    # Collapse Sports and then sort them alphabetically
    Sports = paste(unique(Sport) %>% str_sort(), collapse = ", "),
    # All Games Competed in
    Games = paste(Games, "Games", collapse = ", "),
    # Medal Count
    Gold = sum(ifelse(Medal == "Gold",1, 0)),
    Silver = sum(ifelse(Medal == "Silver",1, 0)), 
    Bronze = sum(ifelse(Medal == "Bronze",1, 0))
    ) %>%
  # Ungroup
  ungroup() %>% 
  # Both Seasons
  filter(Season == "Summer, Winter") %>% 
  # Multiple Different Sports
  filter(grepl(",", Sports)) %>% 
  # Left Join Country Name
  left_join(noc_regions, by = "NOC") %>% 
  # Rename Region Country
  rename("Country" = "region") %>% 
  # Only Select Important Countries
  select(Name, Country, Sports, Games, Gold, Silver, Bronze) %>% 
  # Only Unique Columns
  unique()

# Eddy Alvarez
eddy <- c("Eddy Alvarez", "USA", "Baseball, Speed Skating",
          "2014 Winter Games, 2020 Summer Games", 0, 2, 0)

# Attach eddy to both_seasons
both_seasons <- rbind(both_seasons, eddy)

# Fix Paths for Flags
both_seasons$Country <- paste0("Flags and Icons/Flags/", both_seasons$Country)

# GT Table
both_seasons %>% 
gt() %>% 
  # Make Country a Flag
  text_transform(
    locations = cells_body(Country),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0(.x, ".png")
      ))
    }) %>% 
  # Caption
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Data from Kaggle") %>% 
  # Title and Subtitle
  tab_header(
    title = md("**Athletes Winning Medals in the Winter and Summer Olympics**"),
    subtitle = "Excludes Athletes that Competed in Winter Events at the 1920 Summer Olympics") %>% 
  # Change Column Label
  cols_label("Games" = "Games Where the Athlete Won a Medal") %>% 
  # Highlight Eddy
  tab_style(
    # Yellow Highlight
    style = list(
      cell_fill(color = rgb(252,177,49, # That Yellow Color
                            maxColorValue = 255))
    ),
    # Where to Highlight
    locations = cells_body(
      columns = Name,
      rows = Name == "Eddy Alvarez")
  ) %>% 
  # Total Medals Spanner Tab
  tab_spanner(
    label = "Total Medals",
    columns = c(Gold, Silver, Bronze)
  )

# Save
#gtsave("Day 13- Winter and Summer Winners.png")
