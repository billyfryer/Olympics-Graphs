# Olympics Data Viz Project
# Day 1
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R
# Flags from Google

# Libraries
library(tidyverse)
library(ggimage)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data
noc_regions <- read.csv("Data Sets/noc_regions.csv") # Olympic Countries Data

### Manipulate Data
summarized_athletes <- athlete_events %>%
  # Only Summer Games
  filter(Season == "Summer") %>% 
  # Group By Athlete and Nation
  group_by(ID, Name, NOC) %>%
  # Count the Number of Medals per color
  summarize(Bronze = sum(ifelse(Medal == "Bronze",1, 0)),
            Silver = sum(ifelse(Medal == "Silver",1, 0)), 
            Gold = sum(ifelse(Medal == "Gold",1, 0)),
            Years = paste(unique(Year), collapse = ","),
            NumberOfGames = str_count(Years, pattern = ",") + 1,
            Sport = paste(unique(Sport), collapse = ", "),
  ) %>% 
  # Replace NAs with 0s
  replace_na(list(Bronze = 0, Silver = 0, Gold = 0))

# Replace "Equestrianism" with "Equestrian" in Sport Category
summarized_athletes$Sport[summarized_athletes$Sport == "Equestrianism"] <-
  "Equestrian"

# Filter to get only athletes with no medals
no_medals <- summarized_athletes %>% 
  filter(Gold == 0 &
         Silver == 0 &
         Bronze == 0) %>% 
  # Arrange by NumberOfGames (most first)
  arrange(desc(NumberOfGames)) %>% 
  # Make Name in good format for graph:
  # Name,
  # Sport
  mutate(Name = paste0(Name, ",\n", Sport)) %>% 
  # Cutoff to keep number of bars reasonable is 8 Olympiads
  filter(NumberOfGames >= 8)

# Make DF of flags and continents to label bars
flags_df <- data.frame(NOC = c("CAN", "AUT", "PER", "DEN", "ITA"),
                       Flag = c("Flags and Icons/Canada.png", 
                                "Flags and Icons/Austria.png",
                                "Flags and Icons/Peru.png",
                                "Flags and Icons/Denmark.png",
                                "Flags and Icons/Italy.png"),
                       Continent = c("Americas",
                                     "Europe",
                                     "Americas",
                                     "Europe",
                                     "Europe"))

# Left Join flags to no_medals
full_no_medals <- left_join(no_medals, flags_df, by = "NOC")

# Plotting!
# reorder(Name, NumberOfGames) lets you have the x be Name but
# in order of Number of Games
ggplot(full_no_medals, aes(x = reorder(Name, NumberOfGames),
                           y = NumberOfGames,
                           fill = Continent)) +
  # alpha makes bars more see through
  geom_bar(stat = "identity", alpha = 0.5) +
  # Make NumberOfGames Axis split at exactly 0,2,4,6,8,10
  scale_y_continuous(breaks=c(0,2,4,6,8,10)) +
  # All Labels, I wonder if Legend can go here too?
  labs(x = "Athlete",
       y = "Number of Games Competed In",
       title = "Long Time, Lovable Losers",
       subtitle = "Athletes that Never Won A Medal \nBut Competed in Multiple Summer Olympiads",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data from Kaggle",
       fill = "Continent") +
  # Centering Labels
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)) +
  # Scale Colors manually
  scale_fill_manual(values = c("Americas" = "#DF0024",
                               "Europe" = "#0085C7")) +
  # Add Flag Images
  geom_image(aes(image = Flag),
             size = 0.1,
             by = "width") +
  # Make Bars Horizontal
  coord_flip()

# ggsave("Day 1- Lovable Losers.png",
#        width = 6.75,
#        height = 4.75)

