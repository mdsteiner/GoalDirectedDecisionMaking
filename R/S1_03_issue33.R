rm(list = ls())
gc()

# ------------
# DATA ANALYSIS ISSUE #33
#
# Study (working title): Need based decision making in a reinforcement learning task.
# 
# Authors:
#   - Markus D. Steiner (markus.d.steiner@gmail.com)
#   - Nathaniel D. Phillips (Nathaniel.D.Phillips.is@gmail.com)
# 
# I just told Jana about our great results, and one question she asked was if people do better as they
# get experience over games. For example,
# 
#  - Are people in the goal condition more likely to reach the goal on the last game than the first
#    game?
#  - In the EV condition, do people earn more points in later games than early games?
#  - Do people in the goal condition act more in an RSF way in later games than early games?
# 
# Not critical analyses but would I think they would be interesting to see in a plot.
# ------------


# --------------------------
# Section 0: Load Libraries
# --------------------------

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(coin)) install.packages("coin"); library(coin)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.game <- readRDS("data/Study1Data/useData/S1_dataGameLevel.rds")


#  - Are people in the goal condition more likely to reach the goal on the last game than the first
#    game?

mean(df.game$goalReachedRate[df.game$goal.condition == "Goal" & df.game$game == 2])
mean(df.game$goalReachedRate[df.game$goal.condition == "Goal" & df.game$game == 11])

wilcox.test(df.game$goalReachedRate[df.game$goal.condition == "Goal" & df.game$game == 2],
            df.game$goalReachedRate[df.game$goal.condition == "Goal" & df.game$game == 11],
            paired = TRUE)

# yes it seems so but let's check the development from first to last game
(props <- unlist(lapply(2:11,
                       function(x, df){
                         mean(df$goalReachedRate[df$goal.condition == "Goal" & df$game == x])
                         },
                       df = df.game)))

n.games <- 10

plot(1:n.games, props, xlab = "Game Nr", ylab = "Prop Goals Reached",
     main = "Prop Goals Reached Over All Games", type = "b", ylim = c(0, 1),
     pch = 16, col = gray(0.3, .7), lwd = 2, cex = 1.5, cex.lab = 1.5, cex.axis = 1.5)

text(1:n.games, 
     y = props, 
     labels = round(props, 3), pos = 3)


#  - In the No Goal condition, do people earn more points in later games than early games?

mean(df.game$points.cum[df.game$goal.condition == "NoGoal" & df.game$game == 2])
mean(df.game$points.cum[df.game$goal.condition == "NoGoal" & df.game$game == 11])

# yes it seems so but let's check the development from first to last game
(mean.points <- unlist(lapply(2:11,
                        function(x, df){
                          mean(df$points.cum[df$goal.condition == "NoGoal" & df$game == x])
                        },
                        df = df.game)))

n.games <- 10

plot(1:n.games, mean.points, xlab = "Game Nr", ylab = "Mean Points earned",
     main = "Mean Points Earned Over All Games, No Goal Condition", type = "b", ylim = c(0, 120),
     pch = 16, col = gray(0.3, .7), lwd = 2, cex = 1.5, cex.lab = 1.5, cex.axis = 1.5)

text(1:n.games, 
     y = mean.points, 
     labels = round(mean.points, 1), pos = 3)

# or as pirateplot
pirateplot(points.cum ~ game, data = df.game,
           xlab = "Game Nr", ylab = "Mean Points earned",
           main = "Mean Points Earned Over All Games, No Goal Condition")



#  - Do people in the goal condition act more in an RSF way in later games than early games?

mean(df.game$pred.RSF.acc.rate[df.game$goal.condition == "Goal" & df.game$game == 2])
mean(df.game$pred.RSF.acc.rate[df.game$goal.condition == "Goal" & df.game$game == 11])

# no it doesn't seem so but let's check the development from first to last game
(props.RSF.acc <- unlist(lapply(2:11,
                        function(x, df){
                          mean(df$pred.RSF.acc.rate[df$goal.condition == "Goal" & df$game == x])
                        },
                        df = df.game)))

n.games <- 10

plot(1:n.games, props.RSF.acc, xlab = "Game Nr", ylab = "Prop RSF Accuracy",
     main = "Prop RSF Accuracy Over All Games, Goal Condition", type = "b", ylim = c(0, 1),
     pch = 16, col = gray(0.3, .7), lwd = 2, cex = 1.5, cex.lab = 1.5, cex.axis = 1.5)

text(1:n.games, 
     y = props.RSF.acc, 
     labels = round(props.RSF.acc, 3), pos = 3)

# or as pirateplot to have distribution info

pirateplot(pred.RSF.acc.rate ~ game, data = df.game,
           xlab = "Game Nr", ylab = "RSF Prediction Acc Rate",
           main = "RSF Prediction Accuracy Rates Over All Games, Goal Condition")




