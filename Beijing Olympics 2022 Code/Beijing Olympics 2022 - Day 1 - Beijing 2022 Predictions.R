library(tidyverse)
library(rvest)
library(ggplot2)
####################################################
###### Original Kaggle Data
####################################################

# Data From Kaggle
train_data <- read_csv("Data Sets/train.csv")

# Host City Data Website
url <- "https://en.wikipedia.org/wiki/List_of_Olympic_Games_host_cities"

# Scrape Wikipedia Data
url %>%
  read_html() %>% 
  html_table() %>%
  # Second Table
  .[[2]] %>% 
  # First Column is useless so drop it
  .[,-1] %>% 
  # Replace All NAs with an empty string
  mutate_all(na_if, "") %>% 
  # Filter Out when Winter Column is NA
  # This reduces the data to only Winter Games
  filter(!is.na(Winter)) %>% 
  # Select Country and Year Columns
  select(Country, Year) -> hosts_table

# Read NOC Regions Data Set
noc_regions <- read_csv("Data Sets/noc_regions.csv")

# Join Hosts with NOC Regions
hosts_table <- left_join(hosts_table, noc_regions, 
                         by = c("Country" = "region")) %>% 
  # Delete notes Column
  select(-notes) %>% 
# Some countries have multiple NOC Codes due to history
# For example Germany... More spot checking
  filter(Year %in% unique(train_data$Year)) %>% 
  mutate(NOC = case_when(Country == "United States" ~ "USA",
                         Country == "Yugoslavia" ~ "BIH",
                         Country == "Russia[h]" ~ "RUS",
                         Country == "Germany" ~ "GER",
                         Country == "Canada" ~ "CAN",
                         TRUE ~ NOC)) %>% 
  # Only Unique Rows
  unique() %>% 
  # Rename NOC Column Host
  rename("Host" = "NOC") %>% 
  # Select Year and Host Columns
  select(Year, Host)

# Convert Year to Numeric
hosts_table$Year <- as.numeric(hosts_table$Year)

####################################################
# Count Total Number of Medals Per Olympics
####################################################

total_medals <- train_data %>% 
  # Group By Year
  group_by(Year) %>% 
  # Sum Number of Medals Per Year
  summarize(Medal_Total = sum(Medal)) %>%
  # Ungroup
  ungroup()

####################################################
# Expand to include all countries
####################################################

# Read In Test Data Set
test_data <- read_csv("Data Sets/test.csv") %>% 
  # Make a Host Column for Beijing and Add Medal_Total Column
  mutate(Host = case_when(Country == "CHN" ~ 1,
                          TRUE ~ 0),
         Medal_Total = 327) %>% 
  # Drop Medal Column (which were all set to 0)
  select(-Medal)

####################################################
# Expand Grid
####################################################

# Pull All Countries and Years from train_data
Country <- test_data$Country
Year <- unique(train_data$Year)
# Expand to include all combinations of countries and years
full_data <- expand.grid(Country = Country, 
                         Year = Year)
# Join on Hosts
full_data <- left_join(full_data, hosts_table, 
                       by = c("Year" = "Year")) %>%
  # Make Host Variable Numeric
  mutate(Host = case_when(Host == Country ~ 1,
                          TRUE ~ 0))

# Attach Total Medals Awarded
full_data <- left_join(full_data, total_medals,
                       by = c("Year" = "Year"))

# Attach Medals won by a certain country
full_data <- left_join(full_data, train_data,
                      by = c("Year" = "Year",
                             "Country" = "Country")) %>%
  # Replace NAs with 0s
  mutate(Medal = case_when(is.na(Medal) ~ 0,
                           TRUE ~ Medal),
         # Calculate Percent of Total Medals Won by a Country
         Medal_PCT = Medal / Medal_Total) %>% 
  # Deselect Raw Medal count Column
  select(-Medal)

# Clear environment
rm(hosts_table, total_medals, train_data,
   Country, url, Year)
########################################
# Create Logistic Model
########################################
log_model <- glm(Medal_PCT ~ Host + Year + Country,
              data = full_data,
              # This is what makes this model logistic
              family = "binomial")

# Predict Portion of Medals Won
predictions_2022 <- predict(log_model,
                           test_data,
                           "response")
# Add Predictions to test_data and multiply by Medal_Total
# Round to nearest whole number
test_data <- test_data %>% 
  mutate(Pred_Pct = predictions_2022,
         Pred_Medals = round(Pred_Pct * Medal_Total))

predictions <- test_data %>% 
  # Reduce Size of Dataframe
  select(Country, Pred_Medals) %>% 
  # Rename some variables
  rename("Medal_2022" = "Pred_Medals")

# Join predictions with NOC Regions
winning_countries <- left_join(predictions, noc_regions, 
                               by = c("Country" = "NOC")) %>%
  # Drop Notes Column
  select(-notes) %>% 
  # Only Countries with More than 5 Predicted Medals
  filter(Medal_2022 > 5) %>% 
  # Rename Columns
  rename("NOC" = "Country",
         "Country" = "region")

# Get Medal count from 2018
results_2018 <- full_data %>%
  # Filter to 2018
  filter(Year == 2018) %>% 
  # Only Countries with More than 5 Predicted Medals in 2022
  filter(Country %in% winning_countries$NOC) %>% 
  # Make Medal Count a Number
  mutate(Medal_2018 = Medal_Total * Medal_PCT) %>% 
  # Select Country and 2018 Medal Count
  select(Country, Medal_2018)

# Join winnings_countries and results_2018
final_data <- left_join(winning_countries, results_2018,
          by = c("NOC" = "Country")) %>% 
  # Calculate Difference between 2022 and 2018
  mutate(Diff = Medal_2022 - Medal_2018,
         # Make Diff a String with a + sign for positive numbers 
         Diff = case_when(Diff > 0 ~ paste0("+", Diff),
                          TRUE ~ as.character(Diff))) %>% 
  # Reduce Size of Data Frame
  select(Country, NOC, Medal_2018, Medal_2022, Diff) %>%
  # Make Data Frame Longer
  pivot_longer(cols = c(Medal_2018, Medal_2022),
               names_to = "Year",
               values_to = "Count") %>% 
  # Change Year column from Medal_2018 to 2018
  mutate(Year = as.character(str_remove(string = Year, 
                           pattern = "Medal_")))

### Plotting!
ggplot(final_data, aes(x = Country,
                       y = Count,
                       fill = Year,
                       label = Diff)) +
  # Side by Side Bar Chart
  geom_bar(stat = "identity",
           position = "dodge",
           color = "#eae4e2") + # Medium Blue
  geom_label(fill ="#eae4e2", # Medium Blue
             y = 2.5) +
  # Define what colors we want
  scale_fill_manual(labels = c("2018 Results",
                               "2022 Predictions"),
                    values = c("2018" = "#6693aa",
                               "2022" = "#add1eb")) +
  # Labels
  labs(title = "Beijing 2022 Predicted Total Medals",
       subtitle ="Countries with at Least 5 Predicted Medals",
       caption = "Visualization by Billy Fryer @_b4billy_ ~ Data from Kaggle and Wikipedia",
       fill = "") + 
  # Classic Theme
  theme_classic() +
  theme(
    # Center Titles
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    # Change Colors of Background
    plot.background = element_rect(fill = "#98b4cb"),
    panel.background = element_rect(fill = "#98b4cb"),
    legend.background = element_rect(fill = "#98b4cb"),
    # Move Legend to Bottom
    legend.position = "bottom"
  )

# ggsave("Day 1 - Beijing 2022 Predictions.png",
#        width = 5,
#        height = 3)

