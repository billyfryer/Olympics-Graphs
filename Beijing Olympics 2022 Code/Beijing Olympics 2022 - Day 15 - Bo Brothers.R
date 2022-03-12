# Day 15
# Libraries
library(tidyverse)
library(rvest)
library(gt)

### Note: I'm not sure if the code to get this data will work anymore
### This code worked when I originially made the graph, but the links
### May have changed since then.

# Get Bo Brothers Total
url <- "https://www.eurosport.com/olympics/medals/medalist"

# Webscrape Eurosport Website
url %>% 
  read_html() %>% 
  html_table() %>% 
  as.data.frame() -> data

# Change Names of Columns of Data Frame
names(data) <- c("Rank", "Athlete", "Sport", "Gold", "Silver", "Bronze", "Total")

bo_brothers <- data %>% 
  # Only the Bo Brothers
  filter(str_detect(Athlete, "Boe")) %>% 
  # Count Gold, Silver, Bronze, and Total Medals
  summarise(Gold = sum(Gold),
            Silver = sum(Silver),
            Bronze = sum(Bronze),
            Total = sum(Total),
            Country = "Bo Brothers")

# Get Current Medal Standings
url2 <- "https://www.eurosport.com/olympics/medals"
# Webscrape the new website
url2 %>% 
  read_html() %>% 
  html_table() %>% 
  as.data.frame() %>% 
  filter(Country != "") %>% 
  select(-Var.7) -> data2

# Change Names of Columns of Data Frame
names(data2) <- c("Rank", "Country", "Gold", "Silver", "Bronze", "Total")

# Get Rid of Weird Spacing in Country Name
data2$Country <- str_split_fixed(data2$Country, "\n", n = 2) %>% 
  .[,1] %>% 
  str_remove_all(pattern = "\r")

# Combine Bo Brothers and data2
full_data <- bind_rows(bo_brothers, data2) %>% 
  arrange(desc(Gold), desc(Total)) %>% 
  mutate(Rank = 1:nrow(.)) %>% 
  select(Rank, Country, Gold, Silver, Bronze, Total)

full_data %>% 
  # Top 10 "Countries"
  .[1:10,] %>% 
  # GT Table
  gt() %>% 
  # Titles for Table
  tab_header(
    title = md("**Bo Brothers Versus the World**"),
    subtitle = md("Rank Measured by Gold Medals then Total Medals")
  ) %>% 
  # Caption
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) ~ Data from eurosport.com") %>% 
  # More Tab Options
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
  # Center Columns
  cols_align(align = "center") %>% 
  # Highlight Bo Brothers
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
      columns = Country,
      # Where Games row is Tokyo 2020
      rows = Rank == 9)
  )

# gt save
# gtsave("Outputs/Beijing 2022 Output/Day 15 - Bo Brothers Biathlon Dominance.png")
  
         