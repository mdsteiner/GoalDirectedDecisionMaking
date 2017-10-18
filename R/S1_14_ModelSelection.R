rm(list = ls())
gc()

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(afex)) install.packages("afex"); library(afex)
if (!require(coin)) install.packages("coin"); library(coin)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(arm)) install.packages("arm"); library(arm)

if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(sjstats)) install.packages("sjstats"); library(sjstats)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
df.game <- readRDS("data/Study1Data/useData/S1_dataGameLevel.rds")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")

df.trial$id.f <- as.factor(df.trial$id)
df.trial$variance.condition.f <- as.factor(df.trial$variance.condition)
df.trial$goal.condition.f <- as.factor(df.trial$goal.condition)
df.trial$choose.highvar.subj.f <- as.factor(df.trial$choose.highvar)
### condition effect under goal-------------
# models from most complex to least complex

m.rug.1 <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f +
                          (1 + variance.condition.f + goal.condition.f|id.f/game),
                        data = subset(df.trial, overGoal == 0 & game > 1), family = binomial)

summary(m.rug.1)


m.rug.2 <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f +
                         (1|id.f/game),data = subset(df.trial, overGoal == 0 & game > 1),
                       family = binomial)
r2(m.rug.2)
summary(m.rug.2)


m.rug.3 <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f +
                         (1|id.f) + (1|game),data = subset(df.trial, overGoal == 0 & game > 1),
                       family = binomial)

summary(m.rug.3)
r2(m.rug.3)

m.rug.4 <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f +
                         (1|id.f),data = subset(df.trial, overGoal == 0 & game > 1),
                       family = binomial)

summary(m.rug.4)

anova(m.rug.3, m.rug.2)

sjt.lmer(m.rug.3, m.rug.2)


### condition effect above goal-------------
# models from most complex to least complex

m.rag.1 <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f +
                         (1 + variance.condition.f + goal.condition.f|id.f/game),
                       data = subset(df.trial, overGoal == 1 & game > 1), family = binomial)

summary(m.rag.1)


m.rag.2 <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f +
                         (1|id.f/game),data = subset(df.trial, overGoal == 1 & game > 1),
                       family = binomial)

summary(m.rag.2)
r2(m.rag.2)

m.rag.3 <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f +
                         (1|id.f) + (1|game),data = subset(df.trial, overGoal == 1 & game > 1),
                       family = binomial)

summary(m.rag.3)
r2(m.rag.3)
m.rag.4 <- lme4::glmer(high.var.chosen ~ variance.condition.f + goal.condition.f +
                         (1|id.f),data = subset(df.trial, overGoal == 1 & game > 1),
                       family = binomial)

summary(m.rag.4)


anova(m.rag.3, m.rag.2)

sjt.lmer(m.rag.3, m.rag.2)

display(m.rag.2)

### choose highvar ---------------

m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.f + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)
r2(m.chv)

m.chv.n <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.f + (1|id.f/game),
                       data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.n)
r2(m.chv.n)

anova(m.chv, m.chv.n)


m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.f + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv)
r2(m.chv)

m.chv.n <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.f + (1|id.f/game),
                       data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.n)
r2(m.chv.n)


anova(m.chv, m.chv.n)

### model prediction ------------------

m.pa.1 <- lme4::glmer(pred.RSF.acc ~ goal.condition.f + (1|game) + (1|id.f),
                    data = subset(df.trial, game > 1 & pred.EV != pred.RSF), family = binomial)

summary(m.pa.1)
r2(m.pa.1)

m.pa.2 <- lme4::glmer(pred.RSF.acc ~ goal.condition.f + (1|id.f/game),
                    data = subset(df.trial, game > 1 & pred.EV != pred.RSF), family = binomial)

summary(m.pa.2)
r2(m.pa.2)
anova(m.pa.1, m.pa.2)
