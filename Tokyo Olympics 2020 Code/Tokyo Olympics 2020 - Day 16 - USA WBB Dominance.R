# Olympics Data Viz Project
# Day 16
# Billy Fryer

# Data Self Collected from Wikipedia
library(tidyverse)
library(devtools)
#install_github("hrbrmstr/waffle")
library(waffle)

# Data Manipulation
usa_wbb_legacy <- read_csv("Data Sets/USA WBB Legacy.csv")

# Long Data
long_usa_wbb_legacy <- usa_wbb_legacy %>% 
  # COnvert Data to Long
  gather("championship",
         "number",
         -Name) %>% 
  filter(Name != "Coach Dawn Staley")

# This is stored as a variable so I am able to just manipulate 1
# hex code rather than do it 4 times
background <- "#181818"

# Plotting!
ggplot(long_usa_wbb_legacy, aes(values = number,
                                fill = championship)) +
  # Waffle Plot
  geom_waffle( # Color of Line
              color = "darkgrey",
              # Number of Rows in each person
              n_rows = 3,
              # Size of Line between boxes
              size = 0.5) +
  # Choose Fill Colors
  scale_fill_manual(values = 
        c("NCAA Championships" = "#005eb8", # blue
          "Olympics Medals" = "#D6AF36", # gold
          "WNBA Championships" = "#F57B20", # orange
          "FIBA World Championships" ="#a73535", # red
          "International League Championships" = "#74319b", # purple
          "3x3 or Other International Championships" = "#4bceba") #teal-ish
                    ) +
  # For Individual Players, 3 columns
  facet_wrap(~Name, 
             ncol = 3) +
  # Equal coordinates
  coord_equal() +
  # Title and Caption
  labs(title = "Championships Won by Team USA WBB",
       caption = "Viz by Billy Fryer (@_b4billy_) | Data Collected from Wikipedia") +
  # Two columns in Legend
  guides(fill=guide_legend(ncol=2)) +
  theme(# Dark Background,
        plot.background = element_rect(fill = background),
        # Legend Stuff
        legend.title = element_blank(),
        legend.direction = "horizontal",
        legend.position = "bottom",
        legend.background = element_rect(fill = background),
        legend.text = element_text(color = "white"),
        # Title and Caption
        plot.title = element_text(hjust = 0.5,
                                  size = 16,
                                  face = "bold",
                                  color = "white"),
        plot.caption = element_text(hjust = 0.5,
                                    color = "white"),
        # Border Around Panels
        panel.border = element_rect(color = "black",
                                    fill = NA),
        # Fixing little things about each panel
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        # Move Panel Labels
        strip.background = element_blank(),
        strip.text = element_text(color = "white",
                                  face = "bold",
                                  size = 10)
  )

# ggsave("Day 16- USA WBB Dominance.png",
#        width = 5,
#        height = 5)
