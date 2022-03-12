# Day 13
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(ggimage)
library(scales)
library(ggthemes)

# Data from realbiathlon.com API
# json file downloaded to Data Sets Directory
data <- read_json("Data Sets/Women Biathlon 4x6 km Relay.json") %>% 
  .[["relayTeams"]] %>% 
  # R Binds all of the lists together
  do.call(rbind, .) %>%
  # Convert to a Data Frame
  data.frame() 

final_data <- data %>%
  # Select variables (and rename them all in 1 step)
  select(Rank = rank, 
         Country = country, 
         PenaltyTime = totalPenaltyTime, 
         RangeTime = totalRangeTime, 
         Time = totalLoopTime) %>%
  # Columns were lists for some reason? This fixes that
  unnest(cols = c(Rank, Country, PenaltyTime, RangeTime, Time)) %>%
  # Change Roc to ROC
  mutate(Country = case_when(Country == "Roc" ~ "ROC",
                             TRUE ~ Country),
         # Convert Rank, Penalty time, Range Time, Time, to Numeric
         Rank = as.numeric(Rank),
         PenaltyTime = as.numeric(PenaltyTime),
         RangeTime = as.numeric(RangeTime),
         Time = as.numeric(Time),
         # Add Flag Time
         Flag = paste0("Flags and Icons/Flags/", Country, ".png")) %>% 
  # Get Rid of when Time is NA
  filter(!is.na(Time))

### Plotting!
ggplot(final_data, aes(x = Time,
                 y = PenaltyTime)) +
  geom_image(aes(image = Flag),
             size = 0.1) +
  # X and Y axis are time
  scale_x_time(labels = label_time(format = '%H:%M:%S')) +
  scale_y_time(labels = label_time(format = '%M:%S')) +
  # Titles
  labs(title = "Women Biathlon 4x6 km Relay",
       subtitle = "Total Time vs Penalty Time",
       caption = "Visualization by Billy Fryer (@_b4billy_) ~ Data From realbiathlon.com",
       x = "Total Relay Time",
       y = "Penalty Loop Time") +
  # Arrow and Text
  annotate("curve", x = 4324, xend = 4444,
           y = 130, yend = 72, curvature = -0.5,
           colour = "black", size = 1, arrow = arrow()) +
  annotate("label", x = 4324, y = 133, size = 3,
           label = "Czech Republic lost to Ukraine\nBy Less than 2 Seconds!") +
  #Theme
  theme_fivethirtyeight() +
  # Center title, subtitle, and caption, get rid of axis title
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption =  element_text(hjust = 0.5),
        axis.title = element_text())
  
# ggsave(filename = "Outputs/Beijing 2022 Output/Day 13 - W Biathlon Time vs Penalty.png",
#        width = 6.5,
#        height = 4.5)