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

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
df.game <- readRDS("data/Study1Data/useData/S1_dataGameLevel.rds")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")

sink("documents/powerAnalysis.txt")

## Check the probability of selecting high-variance option given that one is BELOW 100 points
# PREDICTION: The probability of selecting the high-variance option given that one is below 100 points (the goal)
#             is HIGHER in the goal than in the no goal condition.

m.rug <- lme4::glmer(high.var.chosen ~ variance.condition + goal.condition + (1|game) + (1|id),
                     data = subset(df.trial, overGoal == 0 & game > 1), family = binomial)

summary(m.rug)

m.rug.power <- powerSim(m.rug)

m.rug.power

m.rug.ci <- confint.merMod(m.rug, method = "boot", parallel = "multicore")

m.rug.ci

## Check the probability of selecting high-variance option given that one is ABOVE 100 points
# PREDICTION: The probability of selecting the high-variance option given that one is above 100 points (the goal)
#             is LOWER in the goal than in the no goal condition.


m.rag <- lme4::glmer(high.var.chosen ~ variance.condition + goal.condition + (1|game) + (1|id),
                     data = subset(df.trial, overGoal == 1 & game > 1), family = binomial)
summary(m.rag)

m.rag.power <- powerSim(m.rag)

m.rag.power

m.rag.ci <- confint.merMod(m.rag, method = "boot", parallel = "multicore")

m.rag.ci


## With a logistic mixed effects model, check whether, in the goal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF.
# PREDICTION: In the goal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF.

m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|id),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)

m.chv.power <- powerSim(m.chv)

m.chv.power

m.chv.ci <- confint.merMod(m.chv, method = "boot", parallel = "multicore")

m.chv.ci

## With a logistic mixed effects model, check whether, in the NoGoal condition, the high variance option is chosen
#  more often when it is rational to do so according to RSF, but to a much less extent than Goal condition.
# Note: We don't have a good psychological explanation for this. It's just a 'strange' correlation between
#   rsf optional options and high ev option that we find in the simulated data, so we want to check it...
# PREDICTION: In the NoGoal condition, the high variance option is chosen with a higher probability when it is rational
#             to do so according to RSF, but only to a small extent if at all.

m.chv.ng <- lme4::glmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|id),
                        data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)

summary(m.chv.ng)

m.chv.ng.power <- powerSim(m.chv.ng)

m.chv.ng.power

m.chv.ng.ci <- confint.merMod(m.chv.ng, method = "boot", parallel = "multicore")

m.chv.ng.ci

## Check behavior when the RSF and high EV model make different choice predictions.
# PREDICTION: When the RSF and high EV model make different choice predictions, the choice prediction from the RSF
#             strategy will be more accurate than the EV strategy for the goal condition. However, in the no-goal
#             condition, the EV strategy will make better predictions.


m.pa <- lme4::glmer(pred.RSF.acc ~ goal.condition + (1|game) + (1|id),
                    data = subset(df.trial, game > 1 & pred.EV != pred.RSF), family = binomial)

summary(m.pa)

m.pa.power <- powerSim(m.pa)

m.pa.power

m.pa.ci <- confint.merMod(m.pa, method = "boot", parallel = "multicore")

m.pa.ci

sink()