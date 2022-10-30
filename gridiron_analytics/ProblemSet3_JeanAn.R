# Gridiron Analytics
# Problem Set #3
# Due: October 25, 2022
# 
# #Submit your answers into an Excel file OR an R script.
# #Use the file pbp_2019.csv for your analysis.  
# #pbp_2019.csv has play-by-play information from every playoff game in the 2019 NFL season.

suppressMessages(library(tidyverse))
pbp_2019 <- read.csv("pbp_2019.csv")

# 1.  In the 2019 playoffs, what was the average number of rush attempts and pass attempts per game?  

# Two ways of doing this

# First way:
pbp_2019 %>% group_by(game_id) %>%
  # count total rush/pass attempt for each game
  summarize(rush = sum(rush_attempt, na.rm = TRUE),
            pass = sum(pass_attempt, na.rm = TRUE)) %>%
  # average the totals from all the games
  summarize(avg_rush = mean(rush), avg_pass = mean(pass))

# Second way:
pbp_2019 %>% 
  # count total rush/pass attempt for all games then divide by number of games
  summarize(games = length(unique(game_id)),
            avg_rush = sum(rush_attempt, na.rm = TRUE) / games,
            avg_pass = sum(pass_attempt, na.rm = TRUE) / games)


# 2.	In the 2019 playoffs, what was the average yards per attempt on rush and pass plays?  

# Find all plays that were either rushing or passing, label them
rush_pass <- pbp_2019 %>%
  mutate(type = ifelse(rush_attempt == 1, "Rush",
                       ifelse(pass_attempt == 1, "Pass", NA))) %>%
  filter(!is.na(type))
# Calculate average yards per attempt for each label
rush_pass %>% group_by(type) %>%
  summarize(avg_yards = mean(yards_gained, na.rm = TRUE))

#   a.	Show the distribution of yards per attempt on rushes and passes on a single visualization. 
#       Remember to always include titles and axis labels.

ggplot(rush_pass, aes(x = yards_gained, color = type)) + geom_density() +
  labs(title = "Distrubution of Yards per Attempt by Play Type",
       x = "Yards Gained", y = "Density", color = "Play Type")


# 3.	In the 2019 playoffs, what was the correlation between Air Yards and EPA on pass plays? 

# Filter for pass plays and remove rows with missing data
air_epa <- pbp_2019 %>%
  filter(pass_attempt == 1 & !is.na(air_yards) & !is.na(epa))
# Calculate correlation coefficient
cor(air_epa$air_yards, air_epa$epa)

#   a.  Plot the relationship in a visualization. Remember to always include titles and axis labels.

ggplot(air_epa, aes(x = air_yards, y = epa)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "Air Yrds vs. Expected Points Added on Pass Plays",
       x = "Air Yards", y = "EPA")


# 4.	In the 2019 playoffs, what percentage of WPA was predicted by a combination of yardline_100 and yards_gained?

# Fit linear model
model <- lm(wpa ~ yardline_100 + yards_gained, data = pbp_2019)
# Get Coefficient of Determination
summary(model)$r.squared
# About 30 percent

#   a.	Which of these factors was more predictive of WPA? Does this make sense? Are both factors statistically significant?

#       Yards gained was much more predictive of WPA. This makes sense, as yards_gained is reflective of
#       the progress on the given play (how much you moved forward), while yardline_100 is reflective of
#       the progress prior to the given play (i.e., you only got to that position on the field because of
#       what you did before this play). Only yards_gained is statistically significant.


# 5.	Using data from the 2019 playoffs, create a visualization of your choosing. 
#     Look into something that interests you, and try to explain the answer as beautifully
#     and thoroughly as possible by creating a compelling visualization. 
#     I’m happy to help with any feedback that you need.

# Get all pass plays that have targeted receiver
passes <- pbp_2019 %>%
  filter(pass_attempt == 1, !is.na(receiver_player_name))
# Calculate Target Share
targets <- passes %>%
  group_by(posteam, receiver_player_name) %>%
  summarize(targets = n(), # Number of times Receiver was targeted
            yards = mean(yards_gained)) %>%
  left_join(passes %>% group_by(posteam) %>%
              # Number of total team passes with targets
              summarize(passes = n()), by = "posteam") %>%
  # Target Share = Targets / Total Team Passes
  mutate(target_rate = targets / passes * 100) %>%
  # Keep receivers with more than 5 total targets
  filter(targets > 5) 

# Only three teams have a Monster Receiver (average yards gained > 12)
ggplot(targets %>% filter(posteam %in% c("KC","SEA","GB")),
       aes(x = target_rate, y = yards, color = posteam, label = receiver_player_name)) +
  # Create scatter plot with very cool two-color points
  geom_point(aes(color = posteam, fill = posteam), shape=21, size = 2, stroke = 2) +
  # Set axis boundaries and label all points with Receiver Name
  xlim(0, 40) + ylim(4,17) + geom_text(hjust = 0.5, vjust = -1, show.legend = FALSE) +
  labs(title = "Receiver Target Share vs. Yards Gained in 2019 NFL Playoffs",
       subtitle = "Green Bay optimized Target Share for Monster Receiver*",
       x = "Target Share (%)", y = "Average Yards Gained",
       caption = "*Receiver with Average Yards Gained > 12") +
  # Use one of primary/secondary team color to set outer boundary color
  scale_color_manual(name = "Team", values = c(KC = "#E31837", SEA = "#69BE28", GB = "#FFB612")) +
  # Use other primary/secondary team color to set inner fill color
  scale_fill_manual(name = "Team", values = c(KC = "#FFB81C", SEA = "#002244", GB = "#203731"))
