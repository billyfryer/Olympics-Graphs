# Day 5! More W Hockey Data Viz
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggimage)
library(scales)
library(geomtextpath)

# Read in Data
raw_df <- read_csv("Data Sets/Power Play Data in Pool Play W Hockey.csv")

# Rename columns
names(raw_df) <- c("Team", "Opp", "TIPP", "PPG")

# Fix Time... Why is it always time???
# Split TIPP based on the : into 3 parts
# TIPP = Time In Power Play
raw_df$Minutes <- str_split_fixed(string = raw_df$TIPP,
                                  pattern = ":",
                                  n = 3) %>% 
  # Then take the first part and assign it to minutes
  .[,1] %>% 
  # Convert to Numeric
  as.numeric()

# Do it again for seconds
# Split TIPP based on the : into 3 parts
raw_df$Seconds <- str_split_fixed(string = raw_df$TIPP,
                                  pattern = ":",
                                  n = 3) %>% 
  # Then take the second part and assign it to minutes
  .[,2] %>% 
  # Convert to Numeric
  as.numeric()

# Combine Minutes and seconds into seconds
raw_df$TIPP_Sec <- raw_df$Minutes * 60 + raw_df$Seconds

### Join NOCs and Country names ###
# Read in NOC names
noc_regions <- read_csv("Data Sets/noc_regions.csv")

# Join together
df_countries <- left_join(raw_df, noc_regions,
                          by = c("Team" = "NOC")) %>% 
  # Fix Russian Olympic Committee
  mutate(Country = case_when(is.na(region) ~ "Russian Olympic Committee",
                             TRUE ~ region)) %>% 
  # Reduce Size of Data Frame
  select(Country, Opp, PPG, TIPP_Sec) %>% 
  # Create Flags Columns
  mutate(Flag = paste0("Flags and Icons/Flags/", Country, ".png"))


### Plotting!
ggplot(df_countries, aes(x = TIPP_Sec,
                         y = PPG,
                         image = Flag)) +
  # Format x axis is MM:SS Format
  scale_x_time(labels = label_time(format = '%M:%S')) +
  # AB Lines, 1 Goal for 2 Minutes of PP
  geom_labelabline(slope = 1/(2*60), 
                   intercept = 0,
                   label = "1 Goal per 2 PP Minutes",
                   fill = "red") +
  # 1 Goal for 4 Minutes of PP
  geom_labelabline(slope = 1/(4*60), 
                   intercept = 0,
                   label = "1 Goal per 4 PP Minutes",
                   fill = "lightblue") +
  # 1 Goal for 8 Minutes of PP
  geom_labelabline(slope = 1/(8*60), 
                   intercept = 0,
                   label = "1 Goal per 8 PP Minutes",
                   fill = "mediumpurple2") +
  # Plot Flags
  geom_image() +
  # Labels
  labs(title = "Power Play Efficiency in W Hockey Pool Play",
       caption = "Visualization by Billy Fryer (@_b4billy_) ~ Data from Olympics.com",
       x = "Time in Powerplay",
       y = "Powerplay Goals") +
  # Apply bw theme
  theme_bw() +
  # Center Titles
  theme(plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5)
  ) +
  # Arrow and Text
  annotate("segment", x = 600, xend = 697, y = .5, yend = .95,
           colour = "black", size = 1, arrow = arrow()) +
  annotate("label", x = 600, y = .5, label = "USA vs CAN")

# ggsave("Day 5- Power Play Efficiency W Hockey.png",
#        width = 6.75,
#        height = 6.5)

### Plotting Inverse!
ggplot(df_countries, aes(x = PPG,
                         y = TIPP_Sec,
                         image = Flag)) +
  # Format x axis is MM:SS Format
  scale_y_time(labels = label_time(format = '%M:%S')) +
  # AB Lines
  # 2 Minutes of PP per Goal
  geom_labelabline(slope = (2*60), 
                   intercept = 0,
                   label = "2 PP Minutes per PP Goal",
                   fill = "red") +
  # 4 Minutes of PP per Goal
  geom_labelabline(slope = (4*60), 
                   intercept = 0,
                   label = "4 PP Minutes per PP Goal",
                   fill = "lightblue") +
  # 8 Minutes of PP per Goal
  geom_labelabline(slope = (8*60), 
                   intercept = 0,
                   label = "8 PP Minutes per PP Goal",
                   fill = "mediumpurple2") +
  # Plot Flags
  geom_image() +
  # Labels
  labs(title = "Power Play Efficiency in W Hockey Pool Play",
       caption = "Visualization by Billy Fryer (@_b4billy_) ~ Data from Olympics.com",
       y = "Time in Powerplay",
       x = "Powerplay Goals") +
  # Apply bw theme
  theme_bw() +
  # Center Titles
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)
  ) +
  # Arrow and Text
  annotate("segment", y = 600, yend = 697, x = .5, xend = .95,
           colour = "black", size = 1, arrow = arrow()) +
  annotate("label", y = 600, x = .5, label = "USA vs CAN")

# ggsave("Day 5- Power Play Efficiency W Hockey Flipped.png",
#        width = 6.5,
#        height = 6.75)
