# Olympics Data Viz Project
# Day 9
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)
library(gt)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data
noc_regions <- read.csv("Data Sets/noc_regions.csv") # Olympic Countries Data

# Change city name to Athens
athlete_events$City[athlete_events$City == "Athina"]<- "Athens"
athlete_events$City[athlete_events$City == "Rio de Janeiro"]<- "Rio"
athlete_events$City[athlete_events$City == "Antwerpen"]<- "Antwerp"


best_single_games_history <- athlete_events %>% 
  # Summer
  filter(Season == "Summer") %>% 
  # Group By ID and Year
  group_by(ID, Year) %>%
  # Summarize
  summarize(Name = Name,
            Sport = Sport,
            NOC = NOC,
            Games = paste(City, Year),
            Gold = sum(ifelse(Medal == "Gold",1, 0)),
            Silver = sum(ifelse(Medal == "Silver",1, 0)), 
            Bronze = sum(ifelse(Medal == "Bronze",1, 0)),
            TotalMedals = sum(Gold, Silver, Bronze)
  ) %>% 
  # Ungroup
  ungroup() %>%
  # Replace NAs
  replace_na(list(Bronze = 0,
                  Silver = 0,
                  Gold = 0,
                  TotalMedals = 0)) %>%
  # Only Unique Rows
  unique() %>% 
  # Only People Who Medaled
  filter(TotalMedals > 0) %>%
  # Arrange By Gold, then Total Medals,
  # Then Silver, then Bronze
  arrange(desc(Gold), desc(TotalMedals), desc(Silver), desc(Bronze)) %>% 
  # Top 8
  slice(1:8) %>% 
  # Arrange Columns
  select(Name:TotalMedals, Year)

# Add Dressel and McKeon from 2020 to Round Out
# Top 10
dressel2020 <- c("Caeleb Dressel", "Swimming", "USA", "Tokyo 2020", 5, 0, 0, 5, 2021)
mckeon2020 <- c("Emma McKeon", "Swimming", "AUS", "Tokyo 2020", 4, 0, 3, 7, 2021)

# rbind to make 1 data frame
top_10_all_time <- rbind(best_single_games_history, dressel2020, mckeon2020) %>% 
  # Arrange Columns same as before
  arrange(desc(Gold), desc(TotalMedals), desc(Silver), desc(Bronze), desc(Year)) %>% 
  # Get Rid of Year Column
  select(-Year)

# Join with noc_regions to get Country Names
top_10_all_time <- left_join(top_10_all_time, noc_regions, by = "NOC") %>% 
   # Rename region to Country 
  rename("Country" = "region") %>% 
  # Reorder Columns and Drop NOC and notes
  select(Name, Sport, Country, everything(), -NOC, -notes)

# Fix paths for images
top_10_all_time$Country <- paste0("Flags and Icons/", top_10_all_time$Country)
top_10_all_time$Sport <- paste0("Flags and Icons/", top_10_all_time$Sport)

# Data Viz
### Table Making
top_10_all_time %>% 
  gt() %>%
  # Title and Subtitle
  tab_header(
    title = md("**Top 10 Greatest Summer Olympics Performances**"),
    subtitle = "Arranged by Gold Medals, Total Medals, Silver Medals, Bronze Medals"
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Historical Data from Kaggle, Tokyo 2020 Data from olympics.com") %>%
  # Tab Options
  tab_options(
    # Table Font Color
    table.font.color = "navy",
    # Bold Title
    heading.title.font.weight = "bold",
    # Change Subtitle Font Size
    heading.subtitle.font.size = 12,
    # Align Heading
    heading.align = "center",
    # Olympic Blue Lines and Change Bar Width
    table.border.top.color = rgb(0,129,200,
                                 maxColorValue = 255),
    column_labels.border.bottom.color = rgb(0,129,200,
                                            maxColorValue = 255),
    column_labels.border.bottom.width = px(3),
    column_labels.border.top.color = rgb(0,129,200,
                                         maxColorValue = 255),
    column_labels.border.top.width = px(3),
  ) %>% 
  # Change Column Label
  cols_label(
    "TotalMedals" = "Total"
  ) %>%
  # Spanner Bar for Medal Count
  tab_spanner(
    # What Spanner Says
    label = md("**Medal Count**"),
    # What Spanner Covers
    columns = c(Gold, Silver, Bronze, TotalMedals)
  ) %>% 
  # Highlight 2020 Athletes in Yellow
  tab_style(
    # What we did to the cell
    style = list(
      # Make Cell Yellow
      cell_fill(color = rgb(252,177,49, # That Yellow Color
                            maxColorValue = 255))
    ),
    # Which Cells to Apply it to
    locations = cells_body(
      # Name Columns
      columns = Name,
      # Where Games row is Tokyo 2020
      rows = Games == "Tokyo 2020")
  )  %>%
  # Change Country Column to Flags in Directory
  text_transform(
    locations = cells_body(Country),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0(.x, ".png"),
        height = 25
      ))
    })   %>%
  # Change Sport Column to Icon in Directory
  text_transform(
    locations = cells_body(Sport),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0(.x, ".jpg")
      ))
    }) %>%
  gtsave("Day 9- Top 10 Performances.png")
