rm(list = ls())
gc()


# Simulate choice patterns with different strategies and try and recover them

# Load libraries
library(tidyverse)

# Read learning functions
source("r/SampExHeurModel.R")

subjects <- 10
games <- 10
trials <- 25

# Note: Table of means and sds as used in the study

#           Option A      Option B
#         Mean    SD    Mean    SD
# Equal     4     2.5     4     11
# Low     2.5     2.5     4     11
# High      4     2.5   2.5     11
#


m_A <- 4
sd_A <- 2.5

m_B <- 2.5
sd_B <- 11

N_par_v <- 5:15                  # N paramter for SampEx Impression
alpha_par_v <- seq(0.1, 6, .1)     # Alpha parameter for reinforcement learning Impression
phi_par_v <- seq(0.1, 5, .05)  # Phi parameter for softmax choice

dat <- tibble(
  id = rep(1:subjects, each = trials * games), # 20 people per model
  game = rep(rep(1:games, each = trials), subjects), # 10 games
  trial = rep(rep(1:trials, games), subjects), # 25 trials per game
  model = rep(c("SampEx_Heur_Goal", "SampEx_Heur_NoGoal",
                "SampEx_Int_Goal", "RL", "Random"),
            each = subjects / 5 * games * trials),
  goal = rep(c(100, Inf, 100, Inf, Inf), each = subjects / 5 * games * trials),
  rule_Imp = rep(c("SampExHeur", "SampExHeur", "SampExInt", "RL", "none"),
                 each = subjects / 5 * games * trials),
  rule_Choice = rep(c("Softmax", "Softmax", "Softmax", "Softmax", "none"),
                    each = subjects / 5 * games * trials),
  outc_A = round(rnorm(trials * games * subjects, m_A, sd_A )),
  outc_B = round(rnorm(trials * games * subjects, m_B, sd_B )),
  pars_Imp = rep(c(sample(N_par_v, size = subjects / 5 * 3 , replace = TRUE),
               sample(alpha_par_v, size = subjects / 5, replace = TRUE),
               rep(NA, subjects / 5)), each = games * trials),
  pars_Choice = rep(c(sample(phi_par_v, size = subjects / 5 * 4, replace = TRUE),
                  rep(NA, subjects / 5)), each = games * trials),
  selection = NA,
  outcome = NA
  
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
  pars_Imp_i <- dat_subj$pars_Imp[1]
  
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


saveRDS("data/Study1Data/useData/ModelSimDat.rds")
