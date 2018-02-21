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


# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevelEU.rds")

df.trial$id.f <- as.factor(df.trial$id)
df.trial$game.f <- as.factor(df.trial$game)
df.trial$trial.f <- as.factor(df.trial$trial)
df.trial$variance.condition.f <- as.factor(df.trial$variance.condition)
df.trial$goal.condition.f <- as.factor(df.trial$goal.condition)

sink("documents/CIs.txt")

## With a logistic mixed effects model, check whether, in the goal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF.
# PREDICTION: In the goal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF.

print("\nEU\n")
m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)

m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci


m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu + (1|id.f/game),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)

m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci

print("\nPT\n")
m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)

m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci


m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt + (1|id.f/game),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)

m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci



## With a logistic mixed effects model, check whether, in the NoGoal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF, but to a much less extent than Goal condition.
# Note: We don't have a good psychological explanation for this. It's just a 'strange' correlation between
#   rsf optional options and high ev option that we find in the simulated data, so we want to check it...
# PREDICTION: In the NoGoal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF, but only to a small extent if at all.

print("\nEU\n")
m.chv.ng <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu + (1|game) + (1|id.f),
                        data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)

summary(m.chv.ng)

m.chv.ng.ci <- confint.merMod(m.chv.ng, method = "boot", parallel = "multicore")

m.chv.ng.ci


m.chv.ng <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu + (1|id.f/game),
                        data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)

summary(m.chv.ng)

m.chv.ng.ci <- confint.merMod(m.chv.ng, method = "boot", parallel = "multicore")

m.chv.ng.ci

print("\nPT\n")
m.chv.ng <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt + (1|game) + (1|id.f),
                        data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)

summary(m.chv.ng)

m.chv.ng.ci <- confint.merMod(m.chv.ng, method = "boot", parallel = "multicore")

m.chv.ng.ci


m.chv.ng <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt + (1|id.f/game),
                        data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)

summary(m.chv.ng)

m.chv.ng.ci <- confint.merMod(m.chv.ng, method = "boot", parallel = "multicore")

m.chv.ng.ci

sink()