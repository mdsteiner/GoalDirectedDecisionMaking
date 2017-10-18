rm(list = ls())
gc()

# ------------
# DATA ANALYSIS ISSUE 41 Power and Confidence Intervals
#
# Study (working title): Need based decision making in a reinforcement learning task.
# 
# Authors:
#   - Markus D. Steiner (markus.d.steiner@gmail.com)
#   - Nathaniel D. Phillips (Nathaniel.D.Phillips.is@gmail.com)
# 
# Run power analyses for the different models we ran
# ------------

# --------------------------
# Section 0: Load Libraries
# --------------------------

if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(simr)) install.packages("simr"); library(simr)
if (!require(sjstats)) install.packages("sjstats"); library(sjstats)

# load dataframes

df.trial <- readRDS("C:/Users/Markus/Dropbox/Masterarbeit/eegoals/data/Study1Data/useData/S1_dataTrialLevelEU.rds")

df.trial$id.f <- as.factor(df.trial$id)
df.trial$game.f <- as.factor(df.trial$game)
df.trial$trial.f <- as.factor(df.trial$trial)
df.trial$variance.condition.f <- as.factor(df.trial$variance.condition)
df.trial$goal.condition.f <- as.factor(df.trial$goal.condition)

sink("C:/Users/Markus/Dropbox/Masterarbeit/eegoals/documents/CIsEURL.txt")

## With a logistic mixed effects model, check whether, in the goal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF.
# PREDICTION: In the goal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF.
print("EU RL model")

print("")
print("Goal choose risky")
m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu.rl + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)

print("Tjurs D:")
r2(m.chv)

m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci


m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu.rl + (1|id.f/game),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)
print("Tjurs D:")
r2(m.chv)

m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci

print("")
print("No Goal choose risky")

m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu.rl + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv)
print("Tjurs D:")
r2(m.chv)
m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci


m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu.rl + (1|id.f/game),
                     data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv)
print("Tjurs D:")
r2(m.chv)
m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci

print("")
print("Prediction Accuracy")


m.pa <- lme4::glmer(pred.RSF.acc.eu.rl ~ goal.condition.f + (1|game) + (1|id.f),
                    data = subset(df.trial, game > 1 & pred.EV.eu.rl != pred.RSF.eu.rl), family = binomial)

summary(m.pa)
print("Tjurs D:")
r2(m.pa)
m.pa.ci <- confint.merMod(m.pa, method = "boot", parallel = "multicore")

m.pa.ci

m.pa <- lme4::glmer(pred.RSF.acc.eu.rl ~ goal.condition.f + (1|id.f/game),
                    data = subset(df.trial, game > 1 & pred.EV.eu.rl != pred.RSF.eu.rl), family = binomial)

summary(m.pa)
print("Tjurs D:")
r2(m.pa)
m.pa.ci <- confint.merMod(m.pa, method = "boot", parallel = "multicore")

m.pa.ci


sink()