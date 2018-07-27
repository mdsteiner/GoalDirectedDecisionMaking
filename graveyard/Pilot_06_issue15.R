# ----------------------
# Issue # 15: redo the analyses with the proportion of high var chosen predicted by 
# when it's rational to choose high var.
# ----------------------


if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(lsr)) install.packages("lsr"); library(lsr)
if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(pwr)) install.packages("pwr"); library(pwr)

# get data
df <- readRDS("data/PilotData/dataTrialLevelPGetthere.rds")

# ----
# Participant Level Analysis
# ----

# --- Actual Distributions

# aggregate to participant level
df.n <- aggregate(high.var.chosen ~ choose.highvar + workerid, FUN = mean,
                  data = subset(df, condition == 3))

# compute test if proportion of high variance chosen was higher when rational
with(df.n, t.test(high.var.chosen[choose.highvar==1], high.var.chosen[choose.highvar==0], paired = T))
cohensD(high.var.chosen ~ choose.highvar, data = df.n, method = "paired")

# compute parameters and enter them here for power analysis to get n:
# http://www.sample-size.net/sample-size-study-paired-t-test/
sd(df.n$high.var.chosen)
with(df.n, cor(high.var.chosen[choose.highvar==1], high.var.chosen[choose.highvar==0]))

# or simply use gpower which suggests a sample of 67 for a two tailed test and a sign niveau of .05

windows(height = 22, width = 33)
pirateplot(high.var.chosen ~ choose.highvar, data = df.n, ylab = "prop high var chosen",
           xlab = "choose high var (rsf)", main = "Actual Distributions")
mtext(paste("d =", round(cohensD(high.var.chosen ~ choose.highvar, data = df.n, method = "paired"), 2)), 4,
      at = .5, cex = 1.4)

# --- Subjective Distributions

# aggregate to participant level
df.n <- aggregate(high.var.chosen ~ choose.highvar.subj + workerid, FUN = mean,
                  data = subset(df, condition == 3))

# compute test if proportion of high variance chosen was higher when rational

# add NA as value to the participant who had no case when it was optimal to choose the high var option
table(df.n$choose.highvar.subj)
x <- table(df.n$workerid)

for (i in 1:sum(x < 2)){
  add.row <- c(1, as.character(names(x)[which(x < 2)[i]]), NA)
  df.n <- rbind(df.n, add.row)
}

# make sure things are ordered correctly
df.n <- df.n[order(df.n$workerid),]
df.n$high.var.chosen <- as.numeric(df.n$high.var.chosen)
df.n$choose.highvar <- as.numeric(df.n$choose.highvar.subj)

# compute test
with(df.n, t.test(high.var.chosen[choose.highvar.subj == 1], high.var.chosen[choose.highvar.subj == 0],
                  paired = T))

cohensD(high.var.chosen ~ choose.highvar.subj, data = df.n, method = "paired") 

windows(height = 22, width = 33)
pirateplot(high.var.chosen ~ choose.highvar.subj, data = df.n, ylab = "prop high var chosen",
           xlab = "choose high var (rsf)", main = "Subjective Distributions")
mtext(paste("d =", round(cohensD(high.var.chosen ~ choose.highvar.subj, data = df.n, method = "paired"), 2)), 4,
      at = .5, cex = 1.4)

# -----
# Game Level Analysis
# -----

# ----- Actual Distribution For choose.highvar
# aggregate to participant level
df.n <- aggregate(high.var.chosen ~ choose.highvar + workerid + game, FUN = mean,
                  data = subset(df, condition == 3))

# compute a mixed effects model with game and workerid as random effects
#summary(lmer(high.var.chosen ~ 1 + choose.highvar + (1|game) + (1|workerid), data = df.n))
gm <- glmer(high.var.chosen ~ choose.highvar + (1|game) + (1|workerid), data = subset(df, condition == 3), family = binomial)

if (!require(simr)) install.packages("simr"); library(simr)

gm2 <- extend(gm, along = "choose.highvar", n = 20)

gm.pc <- powerCurve(gm)

plot(gm.pc)

# plot it
windows(height = 22, width = 33)
par(mar = c(4.1, 6.5, 3, 1))
pirateplot(high.var.chosen ~ choose.highvar + game, data = df.n, ylab = "prop high var chosen",
           main = "Actual Distributions")

# ----- Subjective Distribution for choose.highvar
# aggregate to participant level
df.n <- aggregate(high.var.chosen ~ choose.highvar.subj + workerid + game, FUN = mean,
                  data = subset(df, condition == 3))

# compute a mixed effects model with game and workerid as random effects
#summary(lmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|workerid), data = df.n))
m <- glmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|workerid), data = subset(df, condition == 3), family = binomial)
summary(m)

m.pc <- powerCurve(m)

plot(m.pc)


# plot it
windows(height = 22, width = 33)
par(mar = c(4.1, 7, 3, 1))
pirateplot(high.var.chosen ~ choose.highvar.subj + game, data = df.n, ylab = "prop high var chosen",
           main = "Subjective Distributions")

