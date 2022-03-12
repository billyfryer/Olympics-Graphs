# Beijing Olympics Day 0 Graph

# Libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)
library(gganimate)

#' Olympic Data right now is impossible to scrape (for now), 
#' So I went to Olympics.com (specifically, https://olympics.com/beijing-2022/olympic-games/en/results/ice-hockey/results-women-gpa-000100-.htm) 
#' And I just copied and pasted the data I wanted in an excel sheet

# Read In Data Set
df <- read_xlsx("Data Sets/CAN vs SUI whockey.xlsx")

#' I want to make this cumulative 
#' (so like P2 includes stats from periods 1 and 2)
#' So let's try to do that!

df2 <- df %>% 
  group_by(Team) %>% 
  # cumsum() is the cumulative sum so it does exactly what I want it to do!
    summarize(G = cumsum(G),
              S = cumsum(S),
  # Going to use Period as a time element later, so can't forget that!
              Period = Period) %>%
  ungroup() %>% 
  # Reorder Variables
  select(Period, Team, G, S)

# We need a Score Variable Too!

# In order to get a stacked bar chart like I want, 
# I need this data to be in a longer format
df3 <- df2 %>%
  # cols = columns I want to make longer
  pivot_longer(cols = c("G", "S"),
               # These rename the columns
               names_to = "Stat",
               values_to = "Value")

# Although Long Data is what we need, We also need the current score of the game
# So we're going to join back on the G from df2
df4 <- left_join(df3, df2, by = c("Period" = "Period",
                                  "Team" = "Team")) %>% 
  # Get Rid of Unnecessary Shots Column
  select(-S)

# Plotting!
ggplot(df4, aes(x = Value,
               y = Team,
               # We want to have the Goals on the left of the bar
               fill = factor(Stat, levels = c("S", "G")))) +
  # Bar Graphs
  geom_bar(stat = "identity",
           position = "stack") +
  # Label with scores
  geom_label(data = df4 %>% filter(Stat == "G"),
             # Labels look like "CAN: 0"
             aes(label = paste0(Team, ": ", G)), 
             vjust = 1.5, 
             fill = "white") + 
  # Colors
  scale_fill_manual(values = c("G" = "red",
                               "S" = "darkgrey")) +
  # Set X lims so that this is (hopefully plug and replace)
  xlim(0,100) +
  # gganimate code!
  transition_states(
    # Variable that you want each state to represent
    Period,
    # How long each transition is
    transition_length = 1,
    # How long we hold each state
    state_length = 3
  ) +
  # Labels
  labs(title = "What You've Already Missed in Beijing 2022",
       subtitle = paste0(unique(df$Team)[1], 
                         " vs ",
                         unique(df$Team)[2],
       " W Hockey Pool Play\nPeriod {closest_state}"),
       caption = "Visualization by Billy Fryer ~ Data from Olympics.com",
       x = "",
       fill = "Stat") +
  # Basic Theme Stuff today. Probably more complex stuff later this Olympics
  theme_classic() +
  # Center title, subtitle, caption and legend
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0.5),
    legend.title = element_text(hjust = 0.5)
  )
# Save Animation as a gif
# anim_save("Day 0- What You've Already Missed in Beijing 2022.gif")
