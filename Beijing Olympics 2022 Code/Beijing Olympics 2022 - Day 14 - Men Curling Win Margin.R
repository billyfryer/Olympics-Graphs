# Day 13
# Libraries
library(tidyverse)
library(ggplot2)
library(ggimage)

# Read in Data
curling_data <- read_csv("Data Sets/Curling Round Robin.csv")
# Change Names of Columns
names(curling_data) <- c("gameID", "Team1", "Team1_Score", "Team2", "Team2_Score")

# Win Margin
curling_data <- curling_data %>% 
  # Calculate Win Margins and Get Opponents
  mutate(Team1_WinMargin = Team1_Score - Team2_Score,
         Team2_WinMargin = Team2_Score - Team1_Score,
         Team1_Opponent = Team2,
         Team2_Opponent = Team1)

# Stack Data to make it longer
Team1 <- curling_data %>% 
  # Select All Variables that start with Team1
  select(starts_with("Team1"))
# Rename Variables to have a common name
names(Team1) <- c("Team", "Score", "Win_Margin", "Opponent")
  
Team2 <- curling_data %>% 
  # Select All Variables that start with Team2
  select(starts_with("Team2"))
# Rename Variables to have a common name
names(Team2) <- c("Team", "Score", "Win_Margin", "Opponent")

# Stack Team1 and Team2 Data Frames on top of one another
long_data <- bind_rows(Team1, Team2) %>% 
  # Get rid of when Score, Team or Opponent is NA
  filter(!is.na(Score) & !is.na(Team) & !is.na(Opponent))  %>%
  # Add on Flag Column
  mutate(Image = paste0("Flags and Icons/Flags/", Opponent, ".png"))

### Plotting
ggplot(long_data, aes(x = Win_Margin,
                      y = Opponent)) +
  # Bar Chart Filled by Win Margin
  geom_col(aes(fill = Win_Margin)) +
  # Labels
  labs(title = "Men's Curling Round Robin Win Margins",
       subtitle = "Lighter Bar Means Greater Win Margin",
       x = "Win Margin",
       caption = "Visualization by Billy Fryer (@_b4billy_) ~ Data from NBC") +
  # Facet By Team into 2 Rows
  facet_wrap(~Team,
             nrow = 2) +
  # Attach Flags to the End of Each Bar to Signify Opponent
  geom_image(aes(x = Win_Margin + (abs(Win_Margin)/Win_Margin * 2),
                 image = Image),
             size = 0.1,
             by = "width") +
  # Dark Theme
  theme_dark() +
  # Center Title, Subtitle and Caption
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        # Get Rid of Y axis labels because we added the flags
        axis.text.y = element_blank(),
        # Get rid of all Axis Ticks
        axis.ticks = element_blank(),
        # No Legend
        legend.position = "none")

# ggsave(filename = "Outputs/Beijing 2022 Output/Day 14 - M Curling Round Robin Win Margins.png",
#        width = 6,
#        height = 4)
