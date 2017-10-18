# ---------------------
# main analysis code of eegoals pilot study
# ---------------------


# get list of data

list.games <- list.files("data/", pattern = "_g.csv")
list.surveys <- list.files("data/", pattern = "_s.csv")

list.games <- paste0("data/", list.games)
list.surveys <- paste0("data/", list.surveys)

df.trial <- readRDS("data/dataTrialLevel.rds")
df.game <- readRDS("data/dataGameLevel.rds")
df.participant <- readRDS("data/dataParticipantLevel.rds")
df.pgetthere <- readRDS("data/dataTrialLevelPGetthere.rds")

library(yarrr)
library(dplyr)
library(BayesFactor)


# ---------------
# game data
# ---------------
char.index <- c(1, 3, 5)

n.high.var <- NULL
n.low.var <- NULL
condition <- NULL

for (ii in 1:length(list.games)){
  temp.game <- read.table(list.games[ii], header = T, sep = ",")
  select.opt <- ifelse(temp.game$selection == 0, "0", substr(temp.game$option.order, char.index[temp.game$selection], char.index[temp.game$selection]))
  select.opt.temp <- as.numeric(select.opt)
  n.high.var.temp <- ifelse(temp.game$condition[1] %in% c(1, 3), sum(select.opt == 3), sum(select.opt == 1))
  n.low.var.temp <- ifelse(temp.game$condition[1] %in% c(1, 3), sum(select.opt == 1), sum(select.opt == 3))
  n.high.var <- c(n.high.var, n.high.var.temp)
  n.low.var <- c(n.low.var, n.low.var.temp)
  
  condition <- c(condition, temp.game$condition[1])
}

var.df <- data.frame(condition, n.high.var, n.low.var)
library(yarrr)
pirateplot(n.high.var ~ condition, data = var.df)
pirateplot(n.low.var ~ condition, data = var.df)

t.test(n.high.var[condition == 2], n.high.var[condition ==4], data = var.df)
t.test(n.high.var[condition == 1], n.high.var[condition ==3], data = var.df)
t.test(n.high.var[condition %in% 1:2], n.high.var[condition %in% 3:4], data = var.df)


# --------------
# p risky chosen distribution for different conditions.
# --------------

temp.game <- read.table(list.games[1], header = T, sep = ",")
temp.game <- temp.game[-which(temp.game$selection == 0),]
all.game.data <- array(NA, c(nrow(temp.game), 8, length(list.games)))
char.index <- c(1, 3, 5)

for (ii in 1:length(list.games)){
  temp.game <- read.table(list.games[ii], header = T, sep = ",")
  select.opt <- ifelse(temp.game$selection == 0, "0", substr(temp.game$option.order, char.index[temp.game$selection], char.index[temp.game$selection]))
  temp.game$select.opt <- select.opt
  temp.game <- temp.game[-which(temp.game$select.opt == "0"),]
  all.game.data[,1,ii] <- temp.game$workerid[1]
  all.game.data[,2,ii] <- temp.game$select.opt
  all.game.data[,3,ii] <- temp.game$trial
  all.game.data[,4,ii] <- temp.game$game
  all.game.data[,5,ii] <- temp.game$outcome
  all.game.data[,6,ii] <- temp.game$condition
  all.game.data[,7,ii] <- temp.game$time
  all.game.data[,8,ii] <- temp.game$points.cum
  
}

select <- NULL
switch <- NULL
condit <- NULL
trials <- NULL
point.cum <- NULL
participant <- NULL
games <- NULL
resp.times <- NULL

for (xx in 1:length(list.games)){
  
  for (gam in 1:6){
    temp.select <- all.game.data[,2,xx][all.game.data[,4,xx] == gam]
    temp.switch <- NA
    condi <- all.game.data[1,6,xx]
    temp.cond <- condi
    points.cum.temp <- all.game.data[,8,xx][all.game.data[,4,xx] == gam]
    trial.temp <- all.game.data[,3,xx][all.game.data[,4,xx] == gam]
    gam.temp <- gam
    participant.temp <- xx
    resp.times.temp <- all.game.data[,7,xx][all.game.data[,4,xx] == gam]
    
    for (sel in 2:length(temp.select)){
      temp.switch <- c(temp.switch, ifelse(temp.select[sel-1] == temp.select[sel], 0, 1)) # 1 for switch
      temp.cond <- c(temp.cond, condi)
      participant.temp <- c(participant.temp, xx)
      gam.temp <- c(gam.temp, gam)
      
    }
    select <- c(select, temp.select)
    switch <- c(switch, temp.switch)
    condit <- c(condit, temp.cond)
    trials <- c(trials, trial.temp)
    point.cum <- c(point.cum, points.cum.temp)
    participant <- c(participant, participant.temp)
    games <- c(games, gam.temp)
    resp.times <- c(resp.times, resp.times.temp)
    
  }
}



df.long <- data.frame(participant, games, condit, select, switch, trials, point.cum, resp.times)
df.long$overGoal <- NA
df.long$overGoal <- ifelse(df.long$condit %in% 3:4 & as.numeric(as.character(point.cum)) >= 135, 1, 0)

resp.times.df <- aggregate(formula = as.numeric(as.character(resp.times)) ~ participant + games, FUN = median, data = df.long)

names(resp.times.df)[3] <- "resp.times"
hist(resp.times.df$resp.times)

min(resp.times.df$resp.times)
max(resp.times.df$resp.times)

resp.times.df$participants[resp.times.df$resp.times < .3]


stuff <- aggregate(formula = switch~participant + games + condit, FUN = mean, data = df.long)
pirateplot(switch~games + condit, data = stuff)

# get participants that never switched
sum(stuff$switch == 0)
table(stuff$participant[which(stuff$switch == 0)])

table(stuff$participant[which(stuff$switch == 1)])
exclude.list <- c(1, 17, 18, 29, 40, 50, 76, 81, 97)
list.games[exclude.list]

stuff <- aggregate(formula = switch~participant + games + condit, FUN = mean, data = subset(df.long, games > 1 & as.numeric(as.character(trials)) >= 40))
pirateplot(switch~games + condit, data = stuff)

t.test(stuff$switch[stuff$condit %in% c(1, 2)], stuff$switch[stuff$condit %in% c(3, 4)])
t.test(stuff$switch[stuff$condit %in% c(1)], stuff$switch[stuff$condit %in% c(3)])
t.test(stuff$switch[stuff$condit %in% c(2)], stuff$switch[stuff$condit %in% c(4)])

switch.aov <- aov(stuff$switch ~ stuff$condit)
summary(switch.aov)
TukeyHSD(switch.aov)


aggregate(formula = switch~condit + overGoal, FUN = mean, data = subset(df.long, games > 1 & condit %in% 3:4 & as.numeric(as.character(trials)) < 40))

aggregate(formula = switch~condit + overGoal, FUN = mean, data = subset(df.long, games > 1 & condit %in% 3:4 & as.numeric(as.character(trials)) >= 40))

n.stuff <- aggregate(formula = switch~condit + overGoal + games, FUN = mean, data = subset(df.long, games > 1 & condit %in% 3:4 & as.numeric(as.character(trials)) >= 40))

pirateplot(switch ~ condit + overGoal, n.stuff)

p.risky.cond1 <- NULL
p.risky.cond2 <- NULL
p.risky.cond3 <- NULL
p.risky.cond4 <- NULL
time.cond1 <- NULL
time.cond2 <- NULL
time.cond3 <- NULL
time.cond4 <- NULL
points.cum.cond1 <- NULL
points.cum.cond2 <- NULL
points.cum.cond3 <- NULL
points.cum.cond4 <- NULL
trial <- NULL
game <- NULL
points.max <- NULL
condition.all <- NULL
game.all <- NULL


for (cond in 1:4){
  p.risky.temp <- NULL
  game.temp <- NULL
  trial.temp <- NULL
  time.temp <- NULL
  points.cum.temp <- NULL
  
  for (ga in 1:6){
    max.trial <- ifelse(ga == 1, 20, 50)
    for (tr in 1:max.trial){
      game.temp <- c(game.temp, ga)
      trial.temp <- c(trial.temp, tr)
      
      ind <- ifelse(ga == 1, tr, (20+ ((ga-2) * max.trial)) + tr)
      p.risky.temp <- c(p.risky.temp, ifelse(cond %in% c(1,3), mean(all.game.data[ind, 2,][all.game.data[ind,6,] == cond] == "3"), mean(all.game.data[ind, 2,][all.game.data[ind,6,] == cond] == "1")))
      time.temp <- c(time.temp, mean(as.numeric(all.game.data[ind,7,][all.game.data[ind,6,] == cond])))
      points.cum.temp <- c(points.cum.temp, mean(as.numeric(all.game.data[ind,8,][all.game.data[ind,6,] == cond])))
      if (tr == max.trial){
       points.max <- c(points.max, as.numeric(all.game.data[ind,8,][all.game.data[ind,6,] == cond]))
       condition.all <- c(condition.all, rep(paste0("cond", cond), length(as.numeric(all.game.data[ind,8,][all.game.data[ind,6,] == cond]))))
       game.all <- c(game.all, rep(paste0("game", ga), length(as.numeric(all.game.data[ind,8,][all.game.data[ind,6,] == cond]))))
      }
    }
  }
  if (cond == 1){
    p.risky.cond1 <- p.risky.temp
    time.cond1 <- time.temp
    points.cum.cond1 <- points.cum.temp
    game <- game.temp
    trial <- trial.temp
  }
  if (cond == 2){
    p.risky.cond2 <- p.risky.temp
    time.cond2 <- time.temp
    points.cum.cond2 <- points.cum.temp
  }
  if (cond == 3){
    p.risky.cond3 <- p.risky.temp
    time.cond3 <- time.temp
    points.cum.cond3 <- points.cum.temp
  }
  if (cond == 4){
    p.risky.cond4 <- p.risky.temp
    time.cond4 <- time.temp
    points.cum.cond4 <- points.cum.temp
  }
}

p.all <- data.frame(condition.all, points.max, game.all)
p.risky.df <- data.frame(game, trial, p.risky.cond1, p.risky.cond2, p.risky.cond3, p.risky.cond4,
                         points.cum.cond1, points.cum.cond2, points.cum.cond3, points.cum.cond4,
                         time.cond1, time.cond2, time.cond3, time.cond4)

#layout.mat <- matrix(1:6, nrow = 6, ncol = 1, byrow = T)
#layout(layout.mat, widths = 2)
#par(mfrow= c(1,1))

# plot the p risky curve for each game and condition
l.colors <- piratepal(palette = 'basel', trans = .4)
for (kk in 1:6){
  plot(1, xlab = "Trial", ylab = "p risky",
       xlim = c(0,max(p.risky.df$trial[p.risky.df$game == kk])), ylim = c(0, 1),
       xaxs = "i", yaxs = "i", cex.lab = 1.5, type = "n")
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$p.risky.cond1[p.risky.df$game == kk],
        type = "l", col = l.colors[1], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$p.risky.cond2[p.risky.df$game == kk],
        type = "l", col = l.colors[2], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$p.risky.cond3[p.risky.df$game == kk],
        type = "l", col = l.colors[3], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$p.risky.cond4[p.risky.df$game == kk],
        type = "l", col = l.colors[5], lwd = 2)
  
  legend("topleft", c("Cond 1: high EV is low var", "Cond 2: high EV is high var", "Cond 3: high EV is low var, goal", "Cond 4: high EV is high var, goal"), lty = 1, lwd = 2,
         col = l.colors[c(1:3, 5)], cex = .74)
}


# plot the response time curve for each game and condition
l.colors <- piratepal(palette = 'basel', trans = .4)
for (kk in 1:6){
  plot(1, xlab = "Trial", ylab = "mean resp-time",
       xlim = c(2,max(p.risky.df$trial[p.risky.df$game == kk])), ylim = c(0, 3),
       xaxs = "i", yaxs = "i", cex.lab = 1.5, type = "n")
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$time.cond1[p.risky.df$game == kk],
        type = "l", col = l.colors[1], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$time.cond2[p.risky.df$game == kk],
        type = "l", col = l.colors[2], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$time.cond3[p.risky.df$game == kk],
        type = "l", col = l.colors[3], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$time.cond4[p.risky.df$game == kk],
        type = "l", col = l.colors[5], lwd = 2)
  
  legend("topleft", c("Cond 1: high EV is low var", "Cond 2: high EV is high var", "Cond 3: high EV is low var, goal", "Cond 4: high EV is high var, goal"), lty = 1, lwd = 2,
         col = l.colors[c(1:3, 5)], cex = .74)
}


# plot the cumulative points curve for each game and condition
l.colors <- piratepal(palette = 'basel', trans = .4)
for (kk in 1:6){
  plot(1, xlab = "Trial", ylab = "cummulative points",
       xlim = c(0,max(p.risky.df$trial[p.risky.df$game == kk])), ylim = c(0, 150),
       xaxs = "i", yaxs = "i", cex.lab = 1.5, type = "n")
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$points.cum.cond1[p.risky.df$game == kk],
        type = "l", col = l.colors[1], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$points.cum.cond2[p.risky.df$game == kk],
        type = "l", col = l.colors[2], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$points.cum.cond3[p.risky.df$game == kk],
        type = "l", col = l.colors[3], lwd = 2)
  
  lines(p.risky.df$trial[p.risky.df$game == kk], p.risky.df$points.cum.cond4[p.risky.df$game == kk],
        type = "l", col = l.colors[5], lwd = 2)
  
  legend("topleft", c("Cond 1: high EV is low var", "Cond 2: high EV is high var", "Cond 3: high EV is low var, goal", "Cond 4: high EV is high var, goal"), lty = 1, lwd = 2,
         col = l.colors[c(1:3, 5)], cex = .74)
}

a <- aov(p.all$points.max ~ p.all$condition.all + p.all$game.all)
summary(a)
TukeyHSD(a)

t.test(p.all$points.max[p.all$condition.all %in% c("cond1", "cond3")], p.all$points.max[p.all$condition.all %in% c("cond2", "cond4")])

# condition 3
mean(p.all$points.max[p.all$condition.all == "cond3"] >= 135)

#condition 4
mean(p.all$points.max[p.all$condition.all == "cond4"] >= 135)

# -----------
# Survey data
# -----------
survey.df <- read.table(list.surveys[1], header = T, sep = ",")
#survey <- matrix(NA, nrow = length(list.surveys), ncol = ncol(temp.surv))

for (jj in 2:length(list.surveys)){
  temp.surv <- read.table(list.surveys[jj], header = T, sep = ",")
  survey.df <- rbind(survey.df, temp.surv)
  
}

table(survey.df$attention.check)
sum(is.na(survey.df$text.attention.check))

table(survey.df$condition)

aggregate(age ~ condition, FUN = mean, data = survey.df)
sum(survey.df$BNT == "25" | survey.df$BNT == "25%", na.rm = T)
head(survey.df)
str(survey.df)

# check education distribution
hist(survey.df$education, breaks = 12)

t.test(survey.df$game.difficulty[survey.df$condition == 1], survey.df$game.difficulty[survey.df$condition == 2])

t.test(survey.df$game.difficulty[survey.df$condition == 3], survey.df$game.difficulty[survey.df$condition == 4])

t.test(survey.df$game.difficulty[survey.df$condition %in% 1:2], survey.df$game.difficulty[survey.df$condition %in% 3:4])

#check the age distribution
hist(survey.df$age)

# show who indicated that an error occurred
table(survey.df$error.or.bugs)
survey.df$describeError[survey.df$error.or.bugs == 2]

# check how many and who answered we shouldn't trust their data
table(survey.df$trust.data)
survey.df$workerid[survey.df$trust.data == 2]

# check how many found the instructions clear
table(survey.df$instructions.clear)

hist(survey.df$caredReachGoal)


table(survey.df$which.strategy)
