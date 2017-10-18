rm(list = ls())
gc()

# -----------------------
# issue #5: How does behavior change from game 1 to game 5?
# -----------------------

# Do variables such as points earned, proportion of reaching the goal, switch rates
# (etc.) change consistently across games from game 1 to game 5? 

library(yarrr)
l.cols.m <- piratepal(palette = 'basel', trans = .1)

df.games <- readRDS("data/dataGameLevel.rds")

# create separate variables for Environment and Goal
df.games$Variance <- ifelse(df.games$condition %in% c(1,3), "low", "high")
df.games$Goal <- ifelse(df.games$condition %in% c(1,2), "no", "yes")

windows(height = 22, width = 33)
# do points earned differ over games?
pirateplot(outcome.sum ~ game + Goal + Variance, data = df.games, ylab = "total points earned")


# do mean switch rates differ over games?
windows(height = 22, width = 33)
pirateplot(mean.switched ~ game + Goal + Variance, data = df.games, ylab = "mean switch rates")

# do median response times differ over games?
windows(height = 22, width = 33)
pirateplot(resp.time.median ~ game + Goal + Variance, data = df.games, ylab = "median response times")

# do mean high EV chosen differ over games?
windows(height = 22, width = 33)
pirateplot(highEV.mean ~ game + Goal + Variance, data = df.games, ylab = "mean high EV chosen")

# do mean mean high variance chosen differ over games?
windows(height = 22, width = 33)
pirateplot(high.var.chosen.mean ~ game + Goal + Variance, data = df.games, ylab = "mean high variance chosen")

# differences in goal reached rates?
df <- aggregate(goalReached ~ game + Variance, FUN = mean, data = df.games)
windows(height = 22, width = 33)
par(mfrow = c(1,1))
plot(1, type = "n", xlab = "Games", ylab = "proportion Goals reached", ylim = c(0,1),
     xlim = c(1, 6), cex = 1.5)
lines(df$game[df$Variance == "high"], df$goalReached[df$Variance == "high"], lwd = 2, col = l.cols.m[1])
lines(df$game[df$Variance == "low"], df$goalReached[df$Variance == "low"], lwd = 2, col = l.cols.m[2])
legend("topright", c("High EV is high variance", "High EV is low variance"), lty = 1, lwd = 2,col = l.cols.m[1:2])

