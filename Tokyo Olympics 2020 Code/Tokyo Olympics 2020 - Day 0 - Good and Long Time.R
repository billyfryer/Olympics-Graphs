# Olympics Data Viz Project
# Day 0
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)
library(ggrepel)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

# Summarize athlete_events to get medal counts by athlete
wide_big_winners <- athlete_events %>%
  # Filter out where medals were not won
  filter(!is.na(Medal)) %>% 
  # Group By Athlete and NOC
  group_by(Name, NOC) %>%
  # Count the Number of Medals per color
  summarize(Bronze = sum(ifelse(Medal == "Bronze", 1, 0)),
            Silver = sum(ifelse(Medal == "Silver", 1, 0)), 
            Gold = sum(ifelse(Medal == "Gold",1, 0)),
  # The Collapse = "," argument makes all years into a string
  # with yearrs seperated by Commas
            Years = paste(unique(Year), collapse = ","),
  # str_count() counts the number of commas in Years,
  # and then add one to get the Number of Games an
  # athlete competed in
            NumberOfGames = str_count(Years, pattern = ",") + 1
            ) %>%
  # Calculate Total # of Medals per athlete
  mutate(TotalMedals = sum(Bronze, Silver, Gold)) %>% 
  # Ungroup
  ungroup() %>%
  # Select Order of Columns
  select(Name, NOC, Years, NumberOfGames, Gold, Silver,
         Bronze,TotalMedals) %>%
  # Reorder Rows
  arrange(desc(TotalMedals), desc(Gold), 
          desc(Silver), desc(Bronze), NOC, Name) %>% 
  # Filter to be 10 Medals & More than 2 Different Games
  filter(TotalMedals >= 10 & NumberOfGames >= 2)

# Make big_winners long
big_winners <- wide_big_winners %>% 
  # Make Data into a longer format
  pivot_longer(cols = c(Gold, Silver, Bronze),
               names_to = "MedalType") %>%
  # Create a Percent Medal Column
  # Number of that certain Medal / Total Medals
  mutate(PMedal = value/TotalMedals)

# Make MedalType a Factor Variable
big_winners$MedalType <- factor(big_winners$MedalType,
                                levels = c("Gold",
                                           "Silver",
                                           "Bronze"))

############################
### Rank By Percent Gold
############################
goldRankings <- big_winners %>% 
  filter(MedalType == "Gold") %>% 
  arrange(desc(PMedal)) %>% 
  # Create GoldPercentRank which assigns a ranking value to
  # All Rows by descending PMedal
  mutate(GoldPercentRank = rank(desc(PMedal))) %>% 
  select(Name, GoldPercentRank, PMedal) %>% 
  rename("PGold" = "PMedal")

########################################################
### Left Join GoldPercentRank Back to big_winners
########################################################
big_winners <- left_join(big_winners, goldRankings, by = "Name")
  
big_winners <- big_winners %>% 
  arrange(GoldPercentRank) %>% 
  select(-PMedal) %>% 
  mutate(PGold = paste0(round(PGold * 100,0),"% Gold")) %>%
  # Filter to limit bars on graph
  filter(GoldPercentRank <= 8) %>%
  # Make Names appear in format:
  # Name,
  # NOC
  mutate(Name = paste(Name, NOC, sep = ",\n"))
  
########################################################
### Plotting
########################################################
ggplot(big_winners, aes(x = reorder(Name, desc(GoldPercentRank)),
                        y = value,
                        fill = MedalType)) +
  # Stacked Bar Chart with stat = "identity" because
  # We want y to be the actual value
  geom_bar(position = "stack", stat = "identity") +
  # Coloring for Bars
  scale_fill_manual(values = c("Gold" = "#D6AF36",
                               "Silver" ="#A7A7AD",
                               "Bronze" ="#824A0D")) +
  # All Labels can go in labs()
  labs(title = "Here For A Good Time... And A Long Time",
       subtitle = "Athletes with More Than 10 Medals and Competed in Multiple Games",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data From Kaggle",
       x = "Athlete",
       y = "Total Medals",
       fill = "Medal Type") +
  # Center title and substitle
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ylim(0,30) +
  # Physically Label Percent Gold
  # data is subsetted b/c we only want PGold to be on there
  # once per person, not 3 times
  geom_label(data = subset(big_winners, MedalType == "Gold"),
            aes(label = PGold),
            y = 20,
            fill = "white") +
  # Make it a Horizontal Bar Chart
  coord_flip()

# ggsave("Day 0- Good and Long Time.png",
#         width = 6.75,
#         height = 4.75)
