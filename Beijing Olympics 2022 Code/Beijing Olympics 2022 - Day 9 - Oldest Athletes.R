# Day 9!
library(tidyverse)
library(gt)

# Read in Athletes Data Set
athletes <- read_csv("Data Sets/athlete_events.csv")

oldest <- athletes %>% 
  # Only Winter Athletes
  filter(Season == "Winter") %>% 
  # Only Gold Medalists
  filter(Medal == "Gold") %>% 
  # Oldest Fist
  arrange(desc(Age)) %>% 
  # Since 2000
  filter(Year >= 2000) %>% 
  # Add on Games Logo
  mutate(Games = paste0("Flags and Icons/Logos/", City, " ", Year, " Logo.png")) %>% 
  # Select Columns
  select(Name, Age, Team, Games, Sport) %>% 
  # Get RId of Duplicates
  unique()

# Attach on 2022 Athletes
athletes_2022 <- data.frame(Name = c("Lindsey Jacobellis", "Nick Baumgartner"),
                            Age = c(36, 40),
                            Team = c("USA", "USA"),
                            Games = rep("Flags and Icons/Logos/Beijing 2022 Logo.png"),
                            Sport = rep("Snowboard Cross")
                            )

# Combine the two data sets
athletes_final <- rbind(oldest, athletes_2022) %>%
  # Oldest First, then Alphabetical
  arrange(desc(Age), desc(Name))

# Making a Table
athletes_final %>% 
  # Over 40
  filter(Age >= 40) %>% 
  # Make a GT Table
  gt() %>% 
  # Title and Subtitle
  tab_header(
    title = md("**Oldest Winter Olympics Gold Medalists Since 2000**"),
    subtitle = md("Data from 2018 Not Available")
  ) %>% 
  # Caption
  tab_source_note(source_note = "Viz by Billy Fryer (@_b4billy_) | Data from Kaggle and teamusa.org") %>%
  # Change Sport Column to Icons in Directory
  text_transform(
    locations = cells_body(Sport),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0("Flags and Icons/Icons/", .x, ".png"),
        height = 50
      ))
    }) %>% 
  # Change Team Column to Flags in Directory
  text_transform(
    locations = cells_body(Team),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = paste0("Flags and Icons/Flags/", .x, ".png"),
        height = 25
      ))
    }) %>% 
  # Change Games Column to Logos in Directory
  text_transform(
    locations = cells_body(Games),
    fn = function(x) {
      # loop over the elements of the column
      map_chr(x, ~ local_image(
        filename = .x,
        height = 50
      ))
    }) %>% 
  tab_options(
    # Align Heading and Table
    heading.align = "center",
    table.align = "center")
# gt save
# gtsave("Outputs/Beijing 2022 Output/Day 9 - Old Gold.png")
  
