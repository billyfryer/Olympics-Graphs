# Day 11!
library(tidyverse)
library(ggplot2)
library(gganimate)
library(scales)

# Read in Data
raw_data <- read_csv("Data Sets/Small Monobob Data.csv") %>% 
  # Combine to make a First and Last Name
  mutate(Name = paste(c_ParticipantFirstName, c_ParticipantLastName)) %>% 
  # Rename some variables to look nicer
  rename("ActualStartOrder" = "n_StartOrder",
         "Time" = "n_TimeAbs") %>% 
  # Convert from Milliseconds to Seconds
  mutate(Time = Time / 1000) %>% 
  # Select Name, Start Order and Time
  select(Name, ActualStartOrder, Time)

# Make a combination of all Names and Numbers 1-Number of Possible Racers
expanded_data <- expand.grid(Name = unique(raw_data$Name), 
            RacersPassed = unique(raw_data$ActualStartOrder)) %>% 
  # Join that with the actual data
  left_join(., raw_data, by = c("Name" ="Name")) %>% 
  # Get Rid of When they start after the number of racers 
  # that have already raced
  filter(ActualStartOrder <= RacersPassed)

Per_racer <- expanded_data %>% 
  # Group By Number of Racers already run
  group_by(RacersPassed) %>% 
  # Take the best 20 times
    slice_min(Time, n = 20) %>% 
  # Ungroup
  ungroup()

# Basic Chart
# X is Time, y is Name sorted by best to worst time, fill is start order
ggplot(Per_racer, aes(x = Time,
                      y = reorder(Name, desc(Time)),
                      fill = ActualStartOrder)) +
  # Bar chart
  geom_col() +
  # gganimate code!
  transition_states(
    # Variable that you want each state to represent
    RacersPassed,
    # How long each transition is
    transition_length = 1,
    # How long we hold each state
    state_length = 1.5
  ) +
  # Zoom in on 255-270 on X Axis
  coord_cartesian(xlim=c(255,270))  +
  # Labels
  labs(title = "First Ever Olympic Monobob Results",
       x = "Combined Time for All Heats",
       y = "",
       caption = "Visualization by Billy Fryer (@_b4billy_) ~ Data from NBC",
       # {closest_state} pulls the number of racers passed from gganimate
       subtitle = "After {closest_state} Sledders") +
  # Change X Axis to be in times
  scale_x_time(labels = label_time(format = '%M:%S')) +
  # No Legend, center titles
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5))

# anim_save("Outputs/Beijing 2022 Output/Day 11- Monobob Results.gif")
