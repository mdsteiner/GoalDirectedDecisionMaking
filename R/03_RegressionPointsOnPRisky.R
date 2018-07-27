rm(list = ls())
gc()

### Regression of points on p risky

library(tidyverse)
library(lme4)
library(sjstats)

df_trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")


# get rid of practice trial and factorize data for use in regression model
df_trial <- df_trial %>%
  filter(game > 1) %>%
  mutate(game = game - 2,
         id.f = as.factor(id),
         goal.condition.f = as.factor(goal.condition),
         variance.condition.f = as.factor(variance.condition),
         overGoal.f = as.factor(overGoal),
         goal.condition.bin = as.factor(case_when(goal.condition == "NoGoal" ~ 0,
                                                  goal.condition == "Goal" ~ 1)),
         goal.dist = (100 - points.cum) / (26 - trial) )

df_trialAgg <- df_trial %>%
  group_by(id, variance.condition, goal.condition, trial) %>%
  summarise(risky_rate = mean(high.var.chosen))


# ----------------------
# Run Regression Model
# ----------------------

# save the results to the documents folder using sink
sink("documents/CI_RegPointsAndTrialOnPRisky.txt")

# mixed effects model with random intercepts for subjects and games
model <- glmer(high.var.chosen ~ goal.condition.bin * overGoal.f + points.cum +
                 (1|id.f/game), data = df_trial, family = "binomial")

summary(model)
r2(model)



# mixed effects model with random intercepts for subjects and games, this time with environments
print("")
print("mixed effects model with random intercepts for subjects and games, this time with environments")
print("")
model_env <- glmer(high.var.chosen ~ goal.condition.bin * overGoal.f + points.cum +
               variance.condition + (1|id.f/game), data = df_trial,
               family = "binomial")

summary(model_env)
print("")
print("Tjur's D:")
r2(model_env)

print("")
print("CI")
print("")
model_env_ci <- confint.merMod(model_env, method = "boot", parallel = "multicore")
model_env_ci

# mixed effects model with random intercepts for subjects and games, with distance from goal and trial as predictors
model_goal_dist <- glmer(high.var.chosen ~ goal.condition.bin * goal.dist + trial +
                     (1|id.f/game), data = df_trial, family = "binomial")

summary(model_goal_dist)
r2(model_goal_dist)

# mixed effects model with random intercepts for subjects and games, with distance from goal and trial as predictors

print("")
print("mixed effects model with random intercepts for subjects and games, with distance from goal and trial as predictors")
print("")
model_goal_dist_env <- glmer(high.var.chosen ~ goal.condition.bin * goal.dist + trial +
                           variance.condition + (1|id.f/game), data = df_trial,
                         family = "binomial")

summary(model_goal_dist_env)
print("")
print("Tjur's D:")
print("")
r2(model_goal_dist_env)

print("")
print("CI")
print("")
model_goal_dist_env_ci <- confint.merMod(model_goal_dist_env, method = "boot",
                                         parallel = "multicore")
model_goal_dist_env_ci

sink()

# Now repeat the analysis but only with the last 10 trials

# mixed effects model with random intercepts for subjects and games
model_subs <- glmer(high.var.chosen ~ goal.condition.bin * overGoal.f + points.cum +
                 (1|id.f/game), data = subset(df_trial, trial >= 15),
               family = "binomial")

summary(model_subs)
r2(model_subs)

