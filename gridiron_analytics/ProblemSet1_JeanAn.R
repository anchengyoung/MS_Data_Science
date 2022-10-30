# Gridiron Analytics
# Problem Set #1
# Due: September 27, 2022
# Student Name: Jean An (cya220)

# Import tidyverse
suppressMessages(library(tidyverse))

## 1. Download overall league season-by-season data
##    from https://www.pro-football-reference.com/years/NFL/

## 2. Read the data into R and View the data
df <- read.csv("ProblemSet1_data.tex", skip = 1, row.names = 1)
View(df)

## 3. Create a basic scatterplot showing every season’s
##    average team Points Per Game from 2006 to 2021.
plot(df$Year, df$PF, 
     # Add plot title
     main = "NFL Scoring By Season (2006-2021)",
     # Add X-axis label, set custom range, remove tick marks
     xlab = "Season", xlim = c(2006,2021), xaxt = "n",
     # Add Y-axis label and set custom range
     ylab = "Team PPG", ylim = c(20,26))
# Add custom X-axis tick marks
axis(1, at = seq(2006, 2021, 1))

## 4. Create a line graph showing every season’s
##    average team Passing Yards Per Game from 2006 to 2021.
plot(df$Year, df$Yds.1, type = "l",
     # Add plot title
     main = "NFL Average Passing Yards By Season (2006-2021)",
     # Add X-axis label, set custom range, remove tick marks
     xlab = "Season", xlim = c(2006,2021), xaxt = "n",
     # Add Y-axis label and set custom range
     ylab = "Team Passing Yards", ylim = c(200,250))
# Add custom X-axis tick marks
axis(1, at = seq(2006, 2021, 1))

## 5. What is the correlation between Team Points Per Game and Passing Net Yards Per Attempt?
# Remove Seasons without NY/A data
df_1 <- df %>% filter(!is.na(NY.A))
# Compute Pearson's Correlation
cor(df_1$PF, df_1$NY.A)

## 6. Remove the columns Year and Teams from the data.
df_2 <- df %>% select(-Year, -Tms)

## 7. Create a correlation matrix plot showing
##    the correlations of the remaining data points.
##    What has the strongest correlation with Points?
# Create correlation matrix
CM <- cor(df_2, use="pairwise.complete.obs")
# Load visualization package
suppressMessages(library(corrplot))
# Visualize correlation matrix
corrplot(CM, type = "upper")
# It looks like the stat with the strongest correlation with Points is Passing Touchdowns
# Other strongly correlated stats include Passing Yards, Penalty Yards, and Total Yards
