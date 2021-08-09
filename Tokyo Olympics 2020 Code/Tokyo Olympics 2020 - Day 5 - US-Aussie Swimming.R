# Olympics Data Viz Project
# Day 5
# Billy Fryer

# Data from Kaggle
# url: https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results/code?datasetId=31029&language=R

# Libraries
library(tidyverse)
library(ggimage)
library(png)

# Read in Data
athlete_events <- read_csv("Data Sets/athlete_events.csv") # Athlete Data

swimmers <- athlete_events %>%
  # Summer Season
  filter(Season == "Summer") %>% 
  # Only Aussies
  filter(NOC %in% c("AUS", "USA")) %>% 
  # Swimming
  filter(Sport == "Swimming") %>% 
  # Year > 2000
  filter(Year >= 2000) %>% 
  # Get Rid of Nonmedal Winners
  filter(!is.na(Medal)) %>% 
  # Group by NOC and Year
  group_by(NOC, Year) %>%
  # Summarize
  summarise(Gold = sum(ifelse(Medal == "Gold",1, 0)),
            Silver = sum(ifelse(Medal == "Silver",1, 0)),
            Bronze = sum(ifelse(Medal == "Bronze",1, 0)),
            TotalMedals = sum(Gold, Silver, Bronze)
            ) %>% 
  # Ungroup
  ungroup() %>% 
  # Arrange by Year
  arrange(Year)

# Get all combos of USA/AUS and Year
empty_df <- expand.grid(Year = unique(swimmers$Year), 
                        NOC = c("AUS", "USA"))

# Join with empty_df we've summarized
complete_data <- left_join(empty_df, swimmers, 
                           by = c("NOC", "Year")) %>% 
  arrange(Year)

# Flags
flags <- data.frame(NOC = c("USA", "AUS"),
                    Flag = c("Flags and Icons/USA.png",
                            "Flags and Icons/Australia.png"))

# Left Join complete_data and flags
full <- left_join(complete_data, flags)

# Make NOC a factor
full$NOC <- factor(full$NOC)

# Make Data Longer
long <- full %>% pivot_longer(cols = c(Gold, Silver, Bronze),
                              names_to = "MedalType")
# Convert NOC  and Yearto Factors
long$NOC <- factor(long$NOC)
long$Year <- factor(long$Year)

# aspect ratio
asp_ratio <- 5/2 

### Plotting!
ggplot(long, aes(x = Year,
                 y = value,
                 fill = MedalType,
                 group = NOC)) +
  geom_bar(position = "stack", 
           stat = "identity"
           ) +
  # Facet by Country, 1 column
  facet_wrap(~NOC,
             ncol = 1,) +
  ylim(0,80) +
  scale_fill_manual(values = c("Gold" = "#D6AF36",
                               "Silver" = "#A7A7AD",
                               "Bronze" =  "#824A0D")) +
  # Putting Flags on the Bars
  geom_image(data = subset(long, MedalType == "Bronze"),
               aes(image = Flag,
                   y = TotalMedals),
             size = 0.125,
             asp = asp_ratio,
             by = "width") +
  # Medal Labels
  geom_label(data = subset(long, MedalType == "Bronze"),
             aes(label = paste("Medals:", TotalMedals, sep = "\n")),
             y = 10,
             fill = "white",
             size = 2.5) +
  labs(title = "Australia / US Swimming Rivalry",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data From Kaggle",
       y = "Medal Count",
       fill = element_blank()) +
  theme(# Center Title and Caption
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        # Get Rid of Faceting Labels
        strip.background = element_blank(),
        strip.text = element_blank(),
        # Get Rid Of Grid Lines
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # Make Everything With A Light Blue Background
        panel.background = element_rect(fill = "#BFD5E3"),
        plot.background = element_rect(fill = "#BFD5E3"),
        legend.background = element_rect(fill = "#BFD5E3"))

# ggsave("Day 5- US-Aussie Swimming.png",
#        width = 5.3,
#        height = 4)
  

             