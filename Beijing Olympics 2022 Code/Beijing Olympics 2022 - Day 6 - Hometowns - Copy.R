# Day 6
library(readxl)
library(tidyverse)
library(gt)
library(emoji)

# Read xlsx sheet
data <- read_xlsx("Data Sets/Team USA Medalists Day 6 Hometowns.xlsx")

clean_data <- data %>% 
  # Pull Gender from event name and then edit it to say Male or Female
  mutate(Gender = str_split_fixed(string = Event, pattern = " ", n = 2) %>% .[,1],
         Gender = case_when(Gender == "Women's" ~ "Female",
                            TRUE ~ "Male"),
         # Medal Emoji
         Medal = emoji::medal(Medal),
         # Delete Gender From Event Name
         Event = str_split_fixed(string = Event, pattern = " ", n = 2) %>% .[,2]) %>% 
  # Reorder Columns
  select(Name, Gender, Discipline, Event, Medal, Home)
  

# Making Table!!!
clean_data %>% 
  # gt Table
  gt() %>%
  # Title and Subtitle
  tab_header(
    title = md("**West vs East Coast Snow**"),
    subtitle = md("USA Medalists that Competed on Snow Through Day 5")
  ) %>% 
  # Footnote
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Data from teamusa.org") %>%
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
  # Change Discipline Column to Icons in Directory
  text_transform(
    locations = cells_body(Discipline),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0("Flags and Icons/Icons/", .x, ".png"),
        height = 25
      ))
    }) %>% 
  # Center Columns
  cols_align(align = "center") %>% 
  # Change Font (Selected Font is from Google)
  opt_table_font(google_font(name = "Acme")) %>% 
  #gtsave
  gtsave("Day 6- West vs East Coast Snow.png")
