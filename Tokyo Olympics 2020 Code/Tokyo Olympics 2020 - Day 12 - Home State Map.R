# Olympics Data Viz Project
# Day 12
# Billy Fryer

# Data from USA Today via Insider.com
# url: https://www.insider.com/states-team-usa-tokyo-olympics-2021-7

# Libraries
library(tidyverse)
library(ggthemes)
library(maps)

# Create Data set
states <- tolower(c("California", "Florida", "Texas",
                    "Colorado", "New York", "Illinois",
                    "Massachusetts", "Georgia", "Pennsylvania",
                    "New Jersey", "Arizona", "Indiana", 
                    "Michigan", "Nevada", "North Carolina",
                    "Ohio", "Minnesota", "Virginia",
                    "Maryland", "Missouri", "Washington",
                    "Alabama", "Hawaii", "Oregon",
                    "South Carolina", "Wisconsin", "Kentucky",
                    "Louisiana", "Tennessee", "Utah",
                    "Connecticut", "Kansas", "Mississippi",
                    "Montana", "Vermont", "Alaska",
                    "Deleware", "Iowa", "Nebraska",
                    "Oklahoma", "Rhode Island", "Arkansas",
                    "Maine", "New Hampshire", "New Mexico",
                    "South Dakota"
))

# Athlete Counts
athletes <- c(126, 56, 32, 
              31, 29, 22, 
              22, 20, 19, 
              17, 16, 16, 
              14, 14, 14,
              14, 13, 13,
              11, 10, 10,
              9, 9, 9,
              7, 7, 6,
              6, 5, 5,
              4, 4, 3,
              3, 3, 2,
              2, 2, 2,
              2, 2, 1,
              1, 1, 1,
              1
)

# Make it a Data Frame
states_data <- data.frame(states, athletes)

# Getting Map Data
states_map <- map_data("state")
total_map <- left_join(states_map, states_data,
                       by = c("region" = "states"))

### Plotting!
ggplot(total_map, aes(x = long, 
                      y = lat, 
                      group = group,
                      fill = athletes)) +
  # This draws the shape given by points
  geom_polygon(colour = "white") +
  # Continuous fill
  scale_fill_continuous(low = "navy",
                        high = "red") +
  # Labels and Titles
  labs(title = "Team USA Tokyo 2020 Athletes by Home State",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data From USA Today via Insider.com",
       fill = "Number of\nAthletes") +
  # Void Theme
  theme_void() +
  theme(
    # Center Title and Caption
    plot.title = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5,
                                vjust = 5),
    # Fill All Backgrounds with White
    plot.background = element_rect(color = "white"),
    panel.background = element_rect(color = "white"),
    # Legend Text Centering
    legend.text = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5),
    # Place Legend at a certain Point
    legend.position = c(.9, .3)
  ) +
  # Add Comment for Alaska and Hawaii
  annotate("text", label = "Alaska: 2 Athletes\nHawaii: 9 Athletes", 
           x = - 117, y = 28.5)


# ggsave("Day 12- Home State Map.png",
#        width = 6,
#        height = 4.75)

