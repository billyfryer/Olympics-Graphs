# Olympics Data Viz Project
# Day 7
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Consecutive Medal Wins

# Libraries
library(tidyverse)
library(ggrepel)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

# Replace some cities
athlete_events$City[athlete_events$City == "Roma"] <- "Rome"
athlete_events$City[athlete_events$City == "Rio de Janeiro"] <- "Rio"
athlete_events$City[athlete_events$City == "Athina"] <- "Athens"
athlete_events$City[athlete_events$Year == 1956] <- "Melbourne/Stockholm"

athlete_medal_count <- athlete_events %>% 
  # Summer
  filter(Season == "Summer") %>% 
  # Group by id and year
  group_by(ID, Year) %>% 
  # Summarize to get medal counts
  summarize(Name = unique(Name),
            Age = unique(Age),
            NOC = unique(NOC),
            Games = paste(unique(Year), unique(City)),
            Sport = paste(unique(Sport), collapse = ","), 
            Gold = sum(ifelse(Medal == "Gold", 1, 0), na.rm = TRUE),
            Silver = sum(ifelse(Medal == "Silver", 1, 0), na.rm = TRUE), 
            Bronze = sum(ifelse(Medal == "Bronze", 1, 0), na.rm = TRUE),
            TotalMedals = sum(Gold, Silver, Bronze, na.rm = TRUE),
) %>%
  # Replace NAs with 0s
  replace_na(list(TotalMedals = 0,
                  Gold = 0,
                  Silver = 0,
                  Bronze = 0)) %>%
  # Only Medal Winners
  filter(TotalMedals != 0) %>% 
  # Ungroup
  ungroup() %>% 
  # Add Highest Medal Type Column
  mutate(MedalType = case_when(Gold >= 1 ~ "Gold",
                               Silver >= 1 ~ "Silver",
                               TRUE ~ "Bronze")) %>% 
  # Arrange Rows
  arrange(ID, Year) %>% 
  # Unique Rows
  unique()

streaks <- athlete_medal_count %>% 
  # group_by ID
  group_by(ID) %>%
  # Count number of rows grouped by ID
  # This gives us the number of consecutive 
  # Olympics each athlete went to
  mutate(NumWins = n(),
         AgeDiff = max(Age) - min(Age)) %>%
  # Ungroup
  ungroup() %>%
  # Only Consecutive Games
  # Range presented due to birthdays that fall during
  # the Games. This didn't seem
  # to be an issue when originally created
  # but will be an issue when Tokyo comes out
  filter(AgeDiff %in% c(15,16,17)) %>% 
  # Where NumWins is 5
  filter(NumWins == 5) %>% 
  # Arrange by Name
  arrange(Name) %>% 
  # Make Name a combo of Name and Sport
  mutate(Name = paste(Name, Sport, sep = ", "))

# MedalType as a Factor
streaks$MedalType <- factor(streaks$MedalType,
                                        level = c("Gold",
                                                  "Silver",
                                                  "Bronze"))

### Plotting
ggplot(streaks, aes(x = reorder(Games, Year), 
                                y = reorder(Name, desc(Year)),
                                color = MedalType)) +
  geom_point(show.legend = FALSE,
             size = 4) +
  scale_color_manual(values = c("Gold" = "#D6AF36",
                               "Silver" = "#A7A7AD",
                               "Bronze" = "#824A0D")) +
  labs(
    title = "Longest Medal Streak by Summer Athletes Regardless of Event",
    subtitle = "Medals Colored as Highest Award the\nAthlete Attained that Olympiad Regardless of Event",
    x = "",
    y = "",
    color = "Highest Medal Won\nIn Specific Year",
    caption = "Viz by Billy Fryer (@_b4billy_) | Data From Kaggle"
  ) +
  theme(
    # Centering Labels and Titles
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5,),
    plot.caption = element_text(hjust = 0.5),
    # No Grid Lines
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    # Make x axis labels vertical and center them 
    # next to the bar
    axis.text.x = element_text(angle = 270, 
                               vjust = 0.5,
                               hjust = 0),
    # Bold y axis title
    axis.title.y = element_text(face = "bold"),
    # Legend Centering
    legend.title = element_text(hjust = 0.5),
    legend.text = element_text(hjust = 0.5),
    legend.justification = "center"
        )

# ggsave("Day 7- Medal Streak.png",
#        width = 9,
#        height = 7)
