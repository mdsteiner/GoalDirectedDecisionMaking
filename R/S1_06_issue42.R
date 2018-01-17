rm(list = ls())
gc()

library(yarrr)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
df.game <- readRDS("data/Study1Data/useData/S1_dataGameLevel.rds")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")

### Quantify difference between participants in goal vs no goal condition who
### indicated they would follow RSF

table(df.participant$goal.condition, df.participant$which.strategy)

data.list <- list(nGoal = 189, nNoGoal = 221, rstGoal = 85, rstNoGoal = 66)

stan.samples <- stan(file = "stan/S1_compare_strategy_proportions.stan",
               data = data.list, chains = 6, control = list(max_treedepth = 20))

# Plotting and summarizing the posterior distribution
stan.samples
rstan::traceplot(stan.samples)
plot(stan.samples)
pairs(stan.samples)

samples.df <- as.data.frame(stan.samples)

mean(samples.df$rateGoal > samples.df$rateNoGoal)

# plot posterior distribution

hist(samples.df$rateGoal, col = transparent("red", .5), xlim = c(0, 1),
     main = "Posteriors of Goal (red) and No Goal (blue)",
     xlab = "p of choosing RSF strategy", ylim = c(0, 1500))
hist(samples.df$rateNoGoal, col = transparent("blue", .5), add = TRUE)
