rm(list = ls())
gc()
# get modeling results
library(tidyverse)
library(knitr)


load("data/Study1Data/useData/ModelFitOptim04.RData")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")

#### Only needed for model_comparison0X.RData files
# 
# subj_fits <- subj_fits %>%
#   mutate(g2 = case_when(model != "Random" ~ g2 - 2 * log(250),
#                         TRUE ~ g2))
# # Get the best model for each subject
# model_best <- subj_fits %>%
#   group_by(id) %>%
#   summarise(
#     N_models = sum(g2 == min(g2)),       # Number of best fitting models (hopefully 1)
#     model_best = model[g2 == min(g2)][1],
#     model_best_g2 = g2[g2 == min(g2)][1],
#     model_best_Imp = pars_Imp_mle[g2 == min(g2)][1],
#     model_best_Choice = pars_Choice_mle[g2 == min(g2)][1]
#   )
# 
# # Combine actual participant conditions with best model
# 
# model_best <- dat %>%
#   group_by(id) %>%
#   summarise(
#     goal.condition = goal.condition[1],
#     condition = condition[1]
#   ) %>% 
#   ungroup() %>%
#   left_join(model_best)   # Add modelling results
# 

subj_fits$variance.condition <- NA
subj_fits$goal.condition <- NA

for (i in seq_len(nrow(subj_fits))){
  subj_fits$variance.condition[i] <- dat$variance.condition[dat$id == subj_fits$id[i]][1]
  subj_fits$goal.condition[i] <- dat$goal.condition[dat$id == subj_fits$id[i]][1]
}

av_dat <- subj_fits %>%
  group_by(model, goal.condition, variance.condition) %>%
  summarise(
    bic = mean(bic)
  ) %>%
  group_by(goal.condition, variance.condition) %>%
  summarise(
    best_model = model[which.min(bic)]
  )

# use these models as target models and only do individual analyses with them
subj_fits <- subj_fits %>%
  filter(model %in% c("Random", "GoalHeur", "ThreshHeur", "RL"))




# Get the best model for each subject
model_best <- subj_fits %>%
  group_by(id) %>%
  summarise(
    N_models = sum(bic == min(bic)),       # Number of best fitting models (hopefully 1)
    model_best = model[bic == min(bic)][1],
    model_best_bic = bic[bic == min(bic)][1],
    model_best_Imp = pars_Imp_mle[bic == min(bic)][1],
    model_best_Choice = pars_Choice_mle[bic == min(bic)][1]
  )

# Combine actual participant conditions with best model

model_best <- dat %>%
  group_by(id) %>%
  summarise(
    goal.condition = goal.condition[1],
    condition = condition[1]
  ) %>%
  ungroup() %>%
  left_join(model_best)   # Add modelling results


model_best <- model_best %>%
  left_join(df.participant[c("id", "variance.condition")], by = "id")
 

# Table of results
kable(table(model_best$model_best, model_best$goal.condition))
kable(round(prop.table(table(model_best$model_best, model_best$goal.condition), 2), 3))

# Table of results
var_cond <- unique(model_best$variance.condition)


for (i in var_cond){
  print(i)
  print(
    kable(round(
      prop.table(table(model_best$model_best[model_best$variance.condition == i],
                     model_best$goal.condition[model_best$variance.condition == i]), 2),
      3))
  )
}


mean(model_best$model_best_Imp[model_best$model_best == "SampEx_Int_Goal"])


### Summary Statistics for the different models

pars_summary <- model_best %>%
  group_by(model_best, goal.condition) %>%
  summarise(bic_mean = mean(model_best_bic),
            bic_sd = sd(model_best_bic),
            par_Imp_mean = mean(model_best_Imp),
            par_Imp_sd = sd(model_best_Imp),
            par_Choice_mean = mean(model_best_Choice),
            par_Choice_sd = sd(model_best_Choice),
            N = n())

kable(pars_summary)

# separated for environments
pars_summary_envs <- model_best %>%
  group_by(model_best, variance.condition, goal.condition) %>%
  summarise(bic_mean = mean(model_best_bic),
            bic_sd = sd(model_best_bic),
            par_Imp_mean = mean(model_best_Imp),
            par_Imp_sd = sd(model_best_Imp),
            par_Choice_mean = mean(model_best_Choice),
            par_Choice_sd = sd(model_best_Choice),
            N = n())

kable(pars_summary_envs)


### Check impact of knowledge of participants

# whichHighEv:
# "I think the option with the higher point variability also had higher values on average." = 1,
# "I think the option with the lower point variability had higher values on average." = 2,
# "I think both options gave the same number of points on average." = 3

# whichStrategy:
# "I always tried to select the box that gives the most points on average." = 1,
# "I first looked at how many clicks I had left and how many points I had. Then, I selected one box or the other." = 2

# caredReachGoal:
# "Not at all" = 0,
# "Somewhat" = 1,
# "Very Much" = 2


model_best$whichHighEV <- NA
model_best$whichStrategy <- NA
model_best$caredReachGoal <- NA

for (i in seq_len(nrow(model_best))){
  model_best$whichHighEV[i] <- df.participant$whichHighEV[df.participant$id == model_best$id[i]]
  model_best$whichStrategy[i] <- df.participant$which.strategy[df.participant$id == model_best$id[i]]
  model_best$caredReachGoal[i] <- df.participant$caredReachGoal[df.participant$id == model_best$id[i]]
}

# cared reach goal
kable(round(table(model_best$model_best, model_best$goal.condition, model_best$caredReachGoal), 3))


# which strategy
kable(round(table(model_best$model_best, model_best$goal.condition, model_best$whichStrategy), 3))

# which high ev
kable(round(table(model_best$model_best, model_best$goal.condition, model_best$whichHighEV, model_best$variance.condition), 3))

mean(model_best$whichHighEV[model_best$variance.condition == "High"] == 1)
mean(model_best$whichHighEV[model_best$variance.condition == "Low"] == 2)
mean(model_best$whichHighEV[model_best$variance.condition == "Equal"] == 3)

model_best$corr_judg <- ifelse((model_best$variance.condition == "High" & model_best$whichHighEV == 1) |
                                 (model_best$variance.condition == "Low" & model_best$whichHighEV == 2) |
                                 (model_best$variance.condition == "Equal" & model_best$whichHighEV == 3),
                               1, 0)

mean(model_best$corr_judg)

# Table of results
var_cond <- unique(model_best$variance.condition)


for (i in var_cond){
  print(i)
  print(
    kable(round(
      prop.table(table(model_best$model_best[model_best$variance.condition == i &
                                               model_best$corr_judg == 1],
                       model_best$goal.condition[model_best$variance.condition == i &
                                                   model_best$corr_judg == 1]), 2),
      3))
  )
}


for (i in var_cond){
  print(i)
  print(
    kable(round(
      prop.table(table(model_best$model_best[model_best$variance.condition == i &
                                               model_best$corr_judg == 0],
                       model_best$goal.condition[model_best$variance.condition == i &
                                                   model_best$corr_judg == 0]), 2),
      3))
  )
}



curve(dnorm(x, 4, 2.5), from = -5, to = 15)
curve(dnorm(x, 2.5, 11), add = TRUE)

seq(5, 10, 0.0001)[which(dnorm(seq(5, 10, 0.0001), 4, 2.5) < dnorm(seq(5, 10, 0.0001), 2.5, 11))[1]]

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")

df.trial <- df.trial %>%
  filter(variance.condition == "Low")

df.trial$need <- NA

for (kk in seq_len(nrow(df.trial))){
  
  if (df.trial$trial[kk] == 1){
    df.trial$need[kk] <- 100
  } else {
    df.trial$need[kk] <- (100 - df.trial$points.cum[kk - 1]) / (26 - df.trial$trial[kk])
  }
  
}

id_vec <- model_best$id[model_best$model_best == "ThreshHeur"]

mean(df.trial$need[df.trial$goal.condition == "Goal"] > 8.5155)


mean(df.trial$selection[df.trial$goal.condition == "Goal"] == 2 & df.trial$need[df.trial$goal.condition == "Goal"] > 8.5155)

mean(df.trial$need[df.trial$goal.condition == "NoGoal"] > 8.5155)


mean(df.trial$selection[df.trial$goal.condition == "NoGoal"] == 2 & df.trial$need[df.trial$goal.condition == "NoGoal"] > 8.5155)

