# Biathlon Data!
library(tidyverse)
library(jsonlite)
library(gt)
library(emoji)

# Data from realbiathlon.com API
# json file downloaded to Data Sets Directory
data <- read_json("Data Sets/Biathlon Mixed Relay Data.json") %>% 
  .[["relayTeams"]] %>% 
  # R Binds all of the lists together
  do.call(rbind, .) %>% 
  data.frame()

# Data Cleaning
data2 <- data %>% 
  # Top 10
  filter(rank <= 10) %>% 
  # Reduce Size of Data Frame
  select(rank, country, result, hitsProne:shots) %>% 
  # _Frac Columns are characters and _Pct Columns are numeric
  mutate(Prone_Frac = paste0(hitsProne, "/", shotsProne),
         Prone_Pct = as.numeric(hitsProne) / as.numeric(shotsProne),
         Standing_Frac = paste0(hitsStanding, "/", shotsStanding),
         Standing_Pct = as.numeric(hitsStanding) / as.numeric(shotsStanding),
         Total_Frac = paste0(hits, "/", shots),
         Total_Pct = as.numeric(hits) / as.numeric(shots)
  ) %>% 
  # Reduce Size of Data Frame Again
  select(rank, country, result, Prone_Frac:Total_Pct) %>% 
  # Arrange by Decending Total Shooting Percent
  arrange(desc(Total_Pct))

data3 <- data2 %>% 
  # Assign Medal based on Rank
  mutate(medal = case_when(rank == 1~ "Gold",
                           rank == 2 ~ "Silver",
                           rank == 3 ~ "Bronze",
                           TRUE ~ as.character(rank)),
         # Convere Medal to emojis
         emoji = case_when(medal %in% c("Gold", "Silver", "Bronze") ~
                             emoji::medal(medal),
                           TRUE ~ medal)) %>% 
  # Reorder columns
  select(country, -c(medal, rank), emoji, dplyr::everything())

### Plotting!
data3 %>% 
  # Make a gt Table
  gt() %>%
  # Title and Subtitle
  tab_header(
    title = md("**Beijing 2022 Biathlon Mixed Relay Shooting Performances**"),
    subtitle = md("Top 10 Teams Shown")
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Data from realbiathlon.com") %>%
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
  # Tab Spanners
  tab_spanner(columns = c(Prone_Frac, Prone_Pct),
              label = "Prone") %>% 
  tab_spanner(columns = c(Standing_Frac, Standing_Pct),
              label = "Standing") %>% 
  tab_spanner(columns = c(Total_Frac, Total_Pct),
              label = "Total") %>% 
  # Change Column Labels
  cols_label(
    "emoji" = "Finish",
    "country" = "Country",
    "result" = "Time",
    "Prone_Frac" = "Frac",
    "Prone_Pct" = "Pct",
    "Standing_Frac" = "Frac",
    "Standing_Pct" = "Pct",
    "Total_Frac" = "Frac",
    "Total_Pct" = "Pct",
  ) %>% 
  # Hide rank column
  cols_hide(c(rank, medal)) %>% 
  # Percent Format
  fmt_percent(columns = c(Prone_Pct, Standing_Pct, Total_Pct)) %>% 
  # Center Columns
  cols_align(align = "center") %>% 
  # Coloring the _Pct columns
  data_color(
    # Which columns to color
    columns = c(Standing_Pct, Prone_Pct, Total_Pct),
    colors = scales::col_numeric(
      # custom defined values - notice that order matters!
      palette = c("#CAF0F8", "#90E0EF",  "#00B4D8",
                  "#0077B6", "#03045E"
                   ),
      domain = NULL
    )
  )

# Save
# gtsave("Day 4- Biathlon Mixed Relay Shooting Performances.png")
