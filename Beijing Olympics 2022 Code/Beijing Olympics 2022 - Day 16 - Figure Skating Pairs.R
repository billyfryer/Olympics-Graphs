# Day 16

# Libraries
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggimage)
library(ggthemes)
library(ggtext)

# Read CSV
data <- read_csv("Data Sets/Figure Skating Pairs.csv") %>% 
  # Change Withdrawel to 0
  mutate(Score = case_when(c_Result == "WDW" ~ 0,
                           TRUE ~ as.numeric(c_Result)),
         # Flag Variable
         Flag = paste0("Flags and Icons/Flags/", c_Name, ".png")) %>% 
  # Rename some columns
  rename("Name" = "c_ParticipantShort",
         "ActualStartOrder" = "n_StartOrder",
         "Country" = "c_Name") %>% 
  # Get Rid of c_Result Column
  select(-c_Result)

# This code function similar to the Monobob code
# See that code for full explanation
expanded_data <- expand.grid(Name = unique(data$Name), 
                             SkatersDone = unique(data$ActualStartOrder)) %>%  
  left_join(., data, by = c("Name" ="Name")) %>% 
  filter(ActualStartOrder <= SkatersDone)

top5_each <- expanded_data %>% 
  # Group By Skaters Done
  group_by(SkatersDone) %>% 
    # Rank each skater in a "live rankings" type way
    mutate(Rank = rank(desc(Score))) %>% 
    # Top 5 for every Skater Raced
    filter(Rank <= 5) %>% 
  ungroup()

# Basic Chart
ggplot(top5_each, aes(x = Rank, y = Score,
                      group = Name, fill = Country)) +
  # Bar Chart
  geom_col(width=0.4) +
  # Corresponding Country Flag
  geom_image(aes(image = Flag),
             size = 0.1) +
  # Label Bars with country name
  geom_text(aes(label = Name,
                color = Country,
                y = Score - 15)) +
  # Fill of Bars
  scale_fill_manual(values = c("China" = "#FFFF00",
                               "ROC" = "#1C3578",
                               "United States" = "#0A3161",
                               "Japan" = "#BC002D",
                               "Georgia" ="#DA291C",
                               "Italy" = "#008C45",
                               "Spain" = "#F1BF00",
                               "Canada" = "#000000",
                               "Czech Republic" = "#11457E",
                               "Austria" = "#EF3340",
                               "Hungary" = "#477050")) +
  # Color of Text
  scale_color_manual(values = c("China" = "#000000",
                               "ROC" = "#FFFFFF",
                               "United States" = "#FFFFFF",
                               "Japan" = "#FFFFFF",
                               "Georgia" ="#FFFFFF",
                               "Italy" = "#FFFFFF",
                               "Spain" = "#000000",
                               "Canada" = "#FFFFFF",
                               "Czech Republic" = "#FFFFFF",
                               "Austria" = "#FFFFFF",
                               "Hungary" = "#FFFFFF")) +
  # gganimate code!
  transition_states(
   # Variable that you want each state to represent
   SkatersDone,
   # How long each transition is
   transition_length = 2,
   # How long we hold each state
   state_length = 3
  ) +
  # Not sure why this is needed, but it is
  coord_flip(clip='off') + 
  # Flip X Axis 
  scale_x_reverse() +
  # Labels
  labs(title = "Pairs Figure Skating Top 5",
       x = "",
       y = "Score",
       caption = "Visualization by Billy Fryer (@_b4billy_) ~ Data from NBC",
       subtitle = "After {closest_state} Pairs") +
  # Attach Theme
  theme_solarized() +
  # Nothing on the y-axis
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        # No Legend
        legend.position = "none",
        # Centered Title, Subtitle, and Caption
        plot.title = element_markdown(hjust = 0.5,
                                  size = 12),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        )

# anim_save("Outputs/Beijing 2022 Output/Day 16 - Figure Skating Pairs.gif")
