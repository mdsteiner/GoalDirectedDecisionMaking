rm(list = ls())
gc()

# ------------
# DATA ANALYSIS ISSUE #40
#
# Study (working title): Need based decision making in a reinforcement learning task.
# 
# Authors:
#   - Markus D. Steiner (markus.d.steiner@gmail.com)
#   - Nathaniel D. Phillips (Nathaniel.D.Phillips.is@gmail.com)
# 
# Look at prediction accuracy values for RSF and EV and if they are higher for the people
# who indicated that they would follow the respective strategy.
# ------------

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(coin)) install.packages("coin"); library(coin)


# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
df.game <- readRDS("data/Study1Data/useData/S1_dataGameLevel.rds")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")

# get list of people in the goal condition who either selected the EV or RSF strategy
EV.ids.goal <- df.participant$id[df.participant$which.strategy == 1 &
                                   df.participant$goal.condition == "Goal"]
RSF.ids.goal <- df.participant$id[df.participant$which.strategy == 2 &
                                    df.participant$goal.condition == "Goal"]

# what are the mean accuracy rates for RSF and EV in the Goal condition
mean(df.participant$pred.RSF.acc.rate[df.participant$goal.condition == "Goal"])
mean(df.participant$pred.EV.acc.rate[df.participant$goal.condition == "Goal"])

# what are the mean accuracy rates for RSF and EV for the people that indicated
# that they would follow the respective strategy
mean(df.participant$pred.RSF.acc.rate[df.participant$which.strategy == 2 &
                                        df.participant$goal.condition == "Goal"])
mean(df.participant$pred.EV.acc.rate[df.participant$which.strategy == 1 &
                                       df.participant$goal.condition == "Goal"])

#plot the differences for the 
pirateplot(pred.RSF.acc.rate ~ which.strategy + goal.condition, data = df.participant)
pirateplot(pred.EV.acc.rate ~ which.strategy + goal.condition, data = df.participant)


m.RSF.1 <- with(subset(df.trial, game > 1 & goal.condition == "Goal" & id %in% RSF.ids.goal),
     mean(pred.RSF.acc[pred.EV != pred.RSF], na.rm = TRUE))
m.RSF.2 <- with(subset(df.trial, game > 1 & goal.condition == "Goal" & id %in% EV.ids.goal),
     mean(pred.RSF.acc[pred.EV != pred.RSF], na.rm = TRUE))

chisq.test(c(m.RSF.1, m.RSF.2))

m.EV.1 <- with(subset(df.trial, game > 1 & goal.condition == "Goal" & id %in% RSF.ids.goal),
     mean(pred.EV.acc[pred.EV != pred.RSF], na.rm = TRUE))
m.EV.2 <- with(subset(df.trial, game > 1 & goal.condition == "Goal" & id %in% EV.ids.goal),
     mean(pred.EV.acc[pred.EV != pred.RSF], na.rm = TRUE))

chisq.test(c(m.EV.1, m.EV.2))
