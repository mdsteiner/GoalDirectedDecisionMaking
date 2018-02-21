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

df.trial <- readRDS("C:/Users/Markus/switchdrive/Masterarbeit/eegoals/data/Study1Data/useData/S1_dataTrialLevel.rds")

df.trial$id.f <- as.factor(df.trial$id)
df.trial$game.f <- as.factor(df.trial$game)
df.trial$trial.f <- as.factor(df.trial$trial)
df.trial$variance.condition.f <- as.factor(df.trial$variance.condition)
df.trial$goal.condition.f <- as.factor(df.trial$goal.condition)

sink("C:/Users/Markus/switchdrive/Masterarbeit/eegoals/documents/CIsInteractionPosthoc.txt")

## With a logistic mixed effects model, check whether, in the goal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF.
# PREDICTION: In the goal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF.
print("normal EV model")

print("")
print("Interaction environment goal")
m.rag <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f + variance.condition.f:goal.condition.f + (1|game) + (1|id.f),
                     data = subset(df.trial, overGoal == 1 & game > 1), family = binomial)

summary(m.rag)


print("Tjurs D:")
r2(m.rag)

m.rag.ci <- confint.merMod(m.rag, method = "boot", parallel = "multicore")

m.rag.ci

sink()