rm(list = ls())
gc()


# Simulate choice patterns with different strategies and try and recover them

# Load libraries
library(tidyverse)
library(yarrr)

# Read learning functions
source("r/modeling_functions.R")

# load modeling and participant level data and bind them to one dataset
load("data/Study1Data/useData/ModelFitOptim01.RData")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")


subj_fits <- subj_fits %>%
  filter(model != "RLGoal")

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


# Prepare parameters to simulate data
subjects <- nrow(model_best)
games <- 10
trials <- 25
varCond <- model_best$variance.condition
n_models <- 1

# Note: Table of means and sds as used in the study

#           Option A      Option B
#         Mean    SD    Mean    SD
# Equal     4     2.5     4     11
# High     2.5    2.5     4     11
# Low       4     2.5   2.5     11
#

# Vectors with environment information

Envs <- c("Equal", "High", "Low")
m_A_envs <- c(4, 2.5, 4)
sd_A_envs <- rep(2.5, 3)
m_B_envs <- c(4, 4, 2.5)
sd_B_envs <- rep(11, 3)



ind <- sapply(varCond, function(x) which(Envs == x))
m_A <- m_A_envs[ind]
sd_A <- sd_A_envs[ind]

m_B <- m_B_envs[ind]
sd_B <- sd_B_envs[ind]

ids <- unique(model_best$id)

dat <- tibble(
  id = rep(ids, each = trials * games), # 20 people per model
  game = rep(rep(1:games, each = trials), subjects), # 10 games
  trial = rep(rep(1:trials, games), subjects), # 25 trials per game
  model = rep(model_best$model_best,
              each = games * trials),
  goal = rep(ifelse(model_best$model_best == "SampEx_Int_Goal", 100, Inf),
             each = games * trials),
  rule_Imp = rep(ifelse(model_best$model_best == "NaturalMean", "Mean",
                        ifelse(model_best$model_best == "SampEx_Int_Goal", "SampExInt",
                               ifelse(model_best$model_best == "RL", "RL", "none"))),
                 each = games * trials),
  rule_Choice = rep(ifelse(model_best$model_best == "Random", "none", "Softmax"),
                    each = games * trials),
  outc_A = as.vector(sapply(seq_len(subjects), function(x) {
    round(rnorm(trials * games, m_A[x], sd_A[x] ))
    })),
  outc_B = as.vector(sapply(seq_len(subjects), function(x) {
    round(rnorm(trials * games, m_B[x], sd_B[x] ))
  })),
  pars_Imp = rep(model_best$model_best_Imp, each = games * trials),
  pars_Choice = rep(model_best$model_best_Choice, each = games * trials),
  selection = NA,
  outcome = NA,
  variance_condition = rep(model_best$variance.condition,
                           each = games * trials),
  goal_condition = rep(model_best$goal.condition,
                       each = games * trials)
  
)


for (i in seq_len(subjects)){
  
  # Get participant id
  id_i <- unique(dat$id)[i]
  
  # Get subject data
  dat_subj <- dat %>% 
    filter(id == id_i)
  
  # Get model
  model_i <- dat_subj$model[1]
  
  # Get goal
  points_goal_i <- dat_subj$goal[1]
  
  # Get rule_Imp
  rule_Imp_i <- dat_subj$rule_Imp[1]
  
  # Get rule_Choice
  rule_Choice_i <- dat_subj$rule_Choice[1]
  
  # Impression parameter
  pars_Imp_i <- c(dat_subj$pars_Imp[1])
  
  # Choice Rule Parameter
  pars_Choice_i <- dat_subj$pars_Choice[1]
  
  # Outcome matrix
  outc_mat <- matrix(c(dat_subj$outc_A, dat_subj$outc_B), ncol = 2)
  
  if(model_i != "Random") {
    
    sim_i <- Model_Sim(rule_Choice = rule_Choice_i,      # Choice rule
                       rule_Imp = rule_Imp_i,            # Impression rule
                       pars_Choice = pars_Choice_i,      # Choice parameter(s)
                       pars_Imp = pars_Imp_i,            # Impression parameter(s)
                       selection_v = dat_subj$selection, # Selection vector
                       outcome_v = dat_subj$outcome,     # Outcome vector
                       trial_v = dat_subj$trial,         # trial_v: Vector of trial numbers
                       game_v = dat_subj$game,           # game_v: Vector of game numbers
                       trial_max = 25,                   # trial_max: Maximum number of trials in task
                       points_goal = points_goal_i,      # points_goal: Points desired at goal. If Infinite, then impressions is based on mean
                       option_n = 2,                     # option_n: Number of options
                       game_n = 10,                      # Number of games
                       outcome_mat = outc_mat)           # Matrix containig the possible outcomes
    
    selections <- sim_i$pred
    outcomes <- sim_i$outcome
    
  }
  
  if(model_i == "Random") {
    
    selections <- sample(c(1, 2), size = nrow(dat_subj), replace = TRUE)
    outcomes <- rep(NA, nrow(dat_subj))
    
    for (sel in seq_along(selections)){
      outcomes[sel] <- outc_mat[sel, selections[sel]]
    }
    
  }
  
  dat$selection[dat$id == id_i] <- selections
  dat$outcome[dat$id == id_i] <- outcomes
  
  
}


dat <- dat %>%
  group_by(id, game, trial, model, goal, rule_Imp, rule_Choice, outc_A, outc_B,
           pars_Imp, pars_Choice, variance_condition, goal_condition) %>%
  summarise(high.var.chosen = selection - 1,
            points.cum = cumsum(outcome),
            selection = selection,
            outcome = outcome) %>%
  ungroup()


for (sub in 1:length(unique(dat$id))){
  
  for (ga in 1:max(dat$game)){
    
    dat[dat$id == unique(dat$id)[sub] & dat$game == ga, "points.cum"] <- 
      with(subset(dat, id ==unique(dat$id)[sub] & game == ga),
           cumsum(outcome))
    
  }
  
}

saveRDS(dat, "data/Study1Data/useData/ParticipantParsSimDat.rds")





