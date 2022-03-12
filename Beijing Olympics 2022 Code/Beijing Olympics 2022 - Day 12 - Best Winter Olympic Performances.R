# Olympics Data Viz Project
# Day 12
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)
library(gt)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data
noc_regions <- read.csv("Data Sets/noc_regions.csv") # Olympic Countries Data


best_single_games_history <- athlete_events %>% 
  # Winter Athletes
  filter(Season == "Winter") %>% 
  # Group By ID and Year
  group_by(ID, Year) %>%
  # Summarize to get Personal Medal Count by Athlete
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
  arrange(desc(TotalMedals), desc(Gold), desc(Silver), desc(Bronze)) %>% 
  # 5 or More TotalMedals
  filter(TotalMedals >= 5) %>% 
  # Arrange Columns
  select(Name:TotalMedals, Year)

# Add 2018 and 2022 athletes
maillet2022 <- c("Quentin Fillon Maillet", "Biathlon", "FRA", "Beijing 2022", 2, 3, 0, 5, 2022)
marit2018 <- c("Marit Bjørgen", "Cross Country Skiing", "NOR", "PyeongChang 2018", 2, 1, 2, 5, 2018)

# rbind to make 1 data frame
top_all_time <- rbind(best_single_games_history, maillet2022, marit2018) %>% 
  # Arrange Columns same as before
  arrange(desc(TotalMedals), desc(Gold), desc(Silver), desc(Bronze), desc(Year)) %>% 
  # Get Rid of Year Column
  select(-Year)

# Join with noc_regions to get Country Names
top_all_time <- left_join(top_all_time, noc_regions, by = "NOC") %>% 
  # Rename region to Country 
  rename("Country" = "region") %>% 
  # Reorder Columns and Drop NOC and notes
  select(Name, Sport, Country, everything(), -NOC, -notes)

# Fix paths for images
top_all_time$Country <- paste0("Flags and Icons/Flags/", top_all_time$Country)
top_all_time$Sport <- paste0("Flags and Icons/Icons/", top_all_time$Sport)

# Data Viz
### Table Making
top_all_time %>% 
  gt() %>%
  # Title and Subtitle
  tab_header(
    title = md("**Greatest Winter Olympics Performances**"),
    subtitle = "Arranged by Total Medals, Gold Medals, Silver Medals, Bronze Medals"
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Historical Data from Kaggle, Beijing 2022 Data from ESPN") %>%
  # Tab Options
  tab_options(
    # Table Font Color
    table.font.color = "#000000", # Red #d71921
    # Bold Title
    heading.title.font.weight = "bold",
    # Change Subtitle Font Size
    heading.subtitle.font.size = 12,
    # Align Heading
    heading.align = "center",
    # Gold Lines and Change Bar Width
    table.border.top.color = "#d71921",
    column_labels.border.top.color = "#f4c300",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.color = "#d71921",
    column_labels.border.bottom.width = px(3),
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
  # Highlight 2022 Athlete in Yellow
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
      rows = Games == "Beijing 2022")
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
        filename = paste0(.x, ".png")
      ))
    }) %>% 
  # Center Columns
  cols_align(align = "center") %>% 
  # Change Font (Selected Font is from Google)
  opt_table_font(google_font(name = "Acme"))

# Save
# gtsave("Outputs/Beijing 2022 Output/Day 12- Best Winter Performances.png")
