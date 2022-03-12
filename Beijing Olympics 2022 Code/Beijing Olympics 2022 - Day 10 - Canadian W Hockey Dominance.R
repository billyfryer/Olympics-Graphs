# Day 10
library(tidyverse)
library(ggplot2)

# Data Reading and Manipulation
raw_data <- read_csv("Data Sets/W Hockey Scores.csv")

# Make Data Long
data1 <- raw_data %>%
  # Select MatchID, Team Name and Score
  select(MatchID, Team1Name, Team1Score) %>% 
  # Rename Variables
  rename("TeamName" = "Team1Name",
        "Score" = "Team1Score")

data2 <- raw_data %>%
  # Select MatchID, Team Name and Score
  select(MatchID, Team2Name, Team2Score) %>% 
  # Rename Variables
  rename("TeamName" = "Team2Name",
        "Score" = "Team2Score")

# Bind rows together (Essentially converting to long data)
long_data <- bind_rows(data1, data2) %>% 
  # No NAs
  filter(!is.na(Score)) %>% 
  # Make Indicator Variables for Canada and Group B
  mutate(isCanada = case_when(TeamName == "Canada" ~ 1,
                              TRUE ~ 0),
         isGroupB = case_when(TeamName %in% c("China", "Japan", "Denmark", 
                                              "Czech Republic", "Sweden") ~ 1,
                              TRUE ~ 0))

# Getting Total Goals Scored by Team
final_data <- long_data %>% 
  # Group By Team
  group_by(TeamName) %>% 
  # Total Goals = Sum of All goals scored by a team
  mutate(TotalGoals = sum(Score)) %>% 
  # Ungroup
  ungroup() %>% 
  # Only select Team, Goals, and Canada/Group B Indicators
  select(TeamName, TotalGoals, isCanada, isGroupB) %>% 
  # Only 1 Row per team
  unique() %>% 
  # Only Canada or Group B Teams
  filter(isCanada | isGroupB) %>% 
  # Label "Canada" or "Group B"
  mutate(Label = case_when(as.logical(isCanada) ~ "Canada",
                           TRUE ~ "Group B"))

### Ploting!
ggplot(final_data, aes(x = TotalGoals,
                       # Make Label a Factor Variable
                       y = factor(Label))) +
  # Bar Chart filled by team
  geom_col(aes(fill = TeamName)) +
  # Scale x Axis to count by 5
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  # Change Colors
  scale_fill_manual(values = c("Canada" = "#999999",
                               "Czech Republic" = "#11457E",
                               "Sweden" = "#FECC02",
                               "Denmark" = "#C8102E",
                               "Japan"= "#FFFFFF",
                               "China" = "#000000"
                               )) +
  # Labels
  labs(title = "Canada vs Group B Total W Hockey Goals",
       subtitle = "Data through the Quarterfinals",
       x = "Goals Scored",
       y = "",
       fill = "",
       caption = "Vizualization by Billy Fryer (@_b4billy_) ~ Data from NBC") +
  # Legend on bottom, Titles centered
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

# ggsave(filename = "Outputs/Beijing 2022 Output/Day 10 - Canada's W Hockey Dominance.png",
#        width = 6,
#        height = 4)
