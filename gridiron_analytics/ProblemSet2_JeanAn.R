# Gridiron Analytics
# Problem Set #2
# Due: October 18, 2022
# 
# #Submit your answers into an R script titled: ProblemSet2_YourFullName.R
# #Use the file pbp_2018.csv for your analysis.  
# #pbp_2018.csv has play-by-play information from every playoff game in the 2018 NFL season.
# 
# 1.	Read the data into R and view the data.

suppressMessages(library(tidyverse))
pbp_2018 <- read.csv("pbp_2018.csv", row.names = 1)
View(pbp_2018)

# 2.	Perform a linear regression that uses 'down' and 'yards_gained' as independent variables and 'epa' as the dependent variable.

model <- lm(epa ~ down + yards_gained, data  = pbp_2018)
summary(model)

# a.	What is the resulting R2? (#Comment your answers)

summary(model)$r.squared

# b.	Adjusted R2?  

summary(model)$adj.r.squared

# c.	What are the coefficients and p-levels for each independent variable?

# Coefficients:
summary(model)$coefficients[,1]
# p-values:
summary(model)$coefficients[,4]

# d.	Can the regression equation be improved by removing any of the independent variables? Why or why not?

# No, the Adjusted R-squared is already pretty close to the Multiple R-squared,
# which suggests that the model is well fit and removing any independent variable
# would unlikely lead to any improvement of the regression model.

# e.	Bonus: What do you make of the results? Any conclusions or areas for future research?
     
# On the surface, it seems like 'down' and 'yards_gained' can explain about 43% of EPA.
# While that is technically true, it is likely because these two variables are already
# baked into the formula used to calculate EP, hence are also part of the formula for EPA.
# Thus, it is no surprise that 'down' and 'yards_gained' are good predictors of EPA.

# 3.	Create a histogram that shows how often passes of each airyards distance are thrown. Remember to include titles and axis labels.
   
ggplot(pbp_2018, aes(x = air_yards)) + geom_histogram(binwidth = 1) +
  labs(title = "Occurance of each Air Yards Distance, 2018 NFL Playoffs",
       x = "Air Yards", y = "Count")

# 4.	Create a density plot with the same data as the histogram. Remember to include titles and axis labels. 

ggplot(pbp_2018, aes(x = air_yards)) + geom_density(fill = "grey35") +
  labs(title = "Distribution of Air Yards Distance, 2018 NFL Playoffs",
       x = "Air Yards", y = "Count")

#   a.	What, if any, different takeaways do you notice from looking at the density plot versus the histogram? Which do you prefer?
     
# The density plot is a lot smoother than the histogram and is less susceptible to some outliers.
# For instance, in the histogram, we can see that 10 yards is thrown more often than 8 or 9 yards,
# and this is likely due to the fact that 10 yards often represent the gain of a new set of downs.
# However, in the density plot, this effect is smoothed out and the slope is continuous downward
# starting from about 5 yards. If the purpose is to be descriptive, I would prefer the histogram,
# but if the purpose is to be predictive, I may prefer the density plot.

# 5.	Create a scatterplot that shows epa vs wpa, colored by play_type. Remember to include titles and axis labels.
   
ggplot(pbp_2018 %>% filter(play_type != "NA"),
       aes(x = epa, y = wpa, color = play_type)) +
  geom_point(alpha = 0.5) + ylim(-0.55, 0.55) +
  labs(title = "EPA vs WPA by Play Type, 2018 NFL Playoffs",
       x = "Expected Points Added (EPA)",
       y = "Win Probability Added (WPA)",
       color = "Play Type")

# 6.	Make the plot as presentable as possible. You will be graded on the quality of your visualization

# Removed one outlier with super low WPA (presumably a game-ending play)
# Made points in plot slightly transparent with alpha  = 0.5
