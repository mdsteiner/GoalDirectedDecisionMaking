## Rerun analyses with participants classified as NOT random

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(afex)) install.packages("afex"); library(afex)
if (!require(coin)) install.packages("coin"); library(coin)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

load("data/Study1Data/useData/model_comparison01.RData")
df.trial <- dat[dat$id %in% unique(dat$id)[model_best$model_best != "Random"],]

df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")
df.participant <- df.participant[model_best$model_best != "Random",]

df.trial$id.f <- as.factor(df.trial$id)
df.trial$variance.condition.f <- as.factor(df.trial$variance.condition)
df.trial$goal.condition.f <- as.factor(df.trial$goal.condition)


# ----------------------
# Section A: Game Data
# ----------------------

### Section A1: Descriptive Analyses

summary(df.trial)

# number of participants per condition
table(df.participant$goal.condition, df.participant$variance.condition)


### Section A2: Inference Statistics

#
## Trial level
# 

## Check the probability of selecting high-variance option given that one is BELOW 100 points
# PREDICTION: The probability of selecting the high-variance option given that one is below 100 points (the goal)
#             is HIGHER in the goal than in the no goal condition.

m.rug <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f + (1|game) + (1|id.f),
                     data = subset(df.trial, overGoal == 0), family = binomial)

summary(m.rug)

# get the odds ration of choosing the high variance option when the variance condition is High compared to Equal
exp(m.rug@beta[2])
# get the odds ration of choosing the high variance option when the variance condition is Low compared to Equal
exp(m.rug@beta[3])
# get the odds ration of choosing the high variance option when the goal condition is NoGoal compared to Goal
exp(m.rug@beta[4])

if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 5.5, width = 8)
}
par(mar=c(5,6.7,3,1.5))
yarrr::pirateplot(risky.ug ~ goal.condition + variance.condition, data = df.participant,
                  ylab = "prop high var chosen", xlab = "Conditions", main = "Risky Under Goal")
# -----------------------

## Check the probability of selecting high-variance option given that one is ABOVE 100 points
# PREDICTION: The probability of selecting the high-variance option given that one is above 100 points (the goal)
#             is LOWER in the goal than in the no goal condition.


m.rag <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f + (1|game) + (1|id.f),
                     data = subset(df.trial, overGoal == 1 & game > 1), family = binomial)

summary(m.rag)


# get the odds ration of choosing the high variance option when the variance condition is High compared to Equal
exp(m.rag@beta[2])
# get the odds ration of choosing the high variance option when the variance condition is Low compared to Equal
exp(m.rag@beta[3])
# get the odds ration of choosing the high variance option when the goal condition is NoGoal compared to Goal
exp(m.rag@beta[4])

if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 5.5, width = 8)
}
par(mar=c(5,6.7,3,1.5))
yarrr::pirateplot(risky.ag ~ goal.condition + variance.condition, data = df.participant,
                  ylab = "prop high var chosen", xlab = "Conditions", main = "Risky Above Goal")
# -----------------------

## With a logistic mixed effects model, check whether, in the goal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF.
# PREDICTION: In the goal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF.

m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)


# get the odds ration of choosing the high variance option when it is rational to do so
exp(m.chv@beta[2])

## With a logistic mixed effects model, check whether, in the NoGoal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF, but to a much less extent than Goal condition.
# Note: We don't have a good psychological explanation for this. It's just a 'strange' correlation between
#   rsf optional options and high ev option that we find in the simulated data, so we want to check it...
# PREDICTION: In the NoGoal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF, but only to a small extent if at all.

m.chv.ng <- lme4::glmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|id.f),
                        data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)

summary(m.chv.ng)

exp(m.chv.ng@beta[2])

# plot this result on participant level

# first aggregate to participant level with choose.highvar as dichotomous variable
df.n <- aggregate(high.var.chosen ~ choose.highvar.subj + id + goal.condition + variance.condition,
                  FUN = mean, data = subset(df.trial, game > 1))

# plot the proportion of high variance chosen separated for the variance, the goal conditions and for whether it
# was, according to rsf, rational to choose the high variance option
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 46)
} else {
  quartz(height = 5.5, width = 11.5)
}
# PREDICTION 1: In the goal conditions, over all variance conditions,the proportion of high variance chosen, on average,
#               is higher when it is rational to do so, compared to when it is not.
#
# PREDICTION 2: This difference does not occur in the no goal condition, i.e. over all variance conditions there is no
#               difference in the proportion of high variance option chosen when it is rational to do so vs when it is
#               not.
par(mar=c(5,7,3,3))
yarrr::pirateplot(high.var.chosen ~ choose.highvar.subj + variance.condition + goal.condition, data = df.n,
                  ylab = "prop high var chosen", xlab = "choose high var subj (rsf)", main = "")
# -----------------------

## Check behavior when the RSF and high EV model make different choice predictions.
# PREDICTION: When the RSF and high EV model make different choice predictions, the choice prediction from the RSF
#             strategy will be more accurate than the EV strategy for the goal condition. However, in the no-goal
#             condition, the EV strategy will make better predictions.

# Note: the next three lines were added after preregistration
# how often do the models make different predictions
with(df.trial, mean(pred.EV != pred.RSF, na.rm = TRUE))
with(df.trial, tapply(pred.EV != pred.RSF, variance.condition, mean, na.rm = T))
with(subset(df.trial, goal.condition == "Goal" & variance.condition == "Low" & trial >= 20), mean(pred.RSF.acc[pred.EV != pred.RSF], na.rm = TRUE))


m.pa <- lme4::glmer(pred.RSF.acc ~ goal.condition.f + (1|game) + (1|id.f),
                    data = subset(df.trial, game > 1 & pred.EV != pred.RSF), family = binomial)

summary(m.pa)

# get the odds ration of RSF prediction accuracy when the condition is goal compared to no goal
exp(m.pa@beta[2])

# aggregate using the dplyr package
df.p <- df.trial %>%
  filter(pred.EV != pred.RSF) %>%
  group_by(id, variance.condition, goal.condition) %>%
  summarise(
    pred.EV.acc.rate = mean(pred.EV.acc, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc, na.rm = TRUE)
  )

if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 5.5, width = 8)
}
par(mar=c(5,6.7,3,1.5))
yarrr::pirateplot(pred.RSF.acc.rate ~ goal.condition + variance.condition, data = df.p,
                  ylab = "RSF corr pred rate", xlab = "Condition", main = "Only Trials where RSF and EV differ")
# -----------------------


# 
## Participant level
#

## Check the proportion of high variance options chosen overall.
# PREDICTION: The proportion of high variance options chosen overall is higher in the goal compared to the no goal
#             conditions.
w.hvo <- coin::wilcox_test(high.var.chosen.rate ~ as.factor(goal.condition), data = df.participant); w.hvo

# effect size r
eff.r.hvo <- as.numeric(w.hvo@statistic@teststatistic / sqrt(nrow(df.participant))); eff.r.hvo

# plot the result
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 11, width = 16)
}
par(mar=c(5,6.7,3,1.5))
yarrr::pirateplot(high.var.chosen.rate ~ goal.condition, data = df.participant,
                  ylab = "prop high var chosen", xlab = "Conditions", main = "Risky Rate Overall")
# -----------------------

## Check the probability of reaching 100 points.
# PREDICTION: The probability of reaching 100 points is higher in the goal vs. the no goal condition.
w.rg <- coin::wilcox_test(goalReachedRate ~ as.factor(goal.condition), data = df.participant); w.rg

# effect size r
eff.r.rg <- as.numeric(w.rg@statistic@teststatistic / sqrt(nrow(df.participant))); eff.r.rg

# plot the result
if (Sys.info()[1] == "Windows"){
  windows(height = 22, width = 33)
} else {
  quartz(height = 5.5, width = 8)
}
par(mar=c(5,6.7,3,1.5))
yarrr::pirateplot(goalReachedRate ~ goal.condition + variance.condition, data = df.participant,
                  ylab = "Reach goal", xlab = "Conditions", main = "Goal Reached Rate")
# -----------------------

