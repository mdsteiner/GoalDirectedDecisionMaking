
# Clear working directory
rm(list = ls())
gc()

# Load libraries
library(tidyverse)
library(parallel)

# SampExHeur Model fitting
source("R/SampExHeurModel.R")


# ---------------------------
# Get data
# ----------------------------

# get trial level data
dat <- as_tibble(readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds"))

# Data cleaning
dat <- dat %>%
  mutate(
    game = game - 1,
    selection = as.numeric(as.character(selection))
  ) %>%
  filter(
    game > 0   # Remove game == 0 (practice games I think)
    ) 

# Number of Subjects
subj_max <- 1 # length(unique(dat$id))

# Game parameters
trial_max <- 25
Goal <- 100

# ---------------------------
# Set up MLE fitting
# ----------------------------

# Grid search parameters
#   The grid search will look over all combinations of these parameters

N_par_v <- 1:15                  # N paramter for SampEx Impression
alpha_par_v <- seq(0, 2, .1)     # Alpha parameter for reinforcement learning Impression
phi_par_v <- seq(0.0, .2, .01)  # Phi parameter for softmax choice

# Vector of all models to fit to each participant
models_to_fit <- c("SampEx_Heur_Goal",    # Sample extrapolation with Heuristic and Goal
                   "SampEx_Heur_NoGoal",  # Sample extrapolation with Heuristic and NoGoal
                   "SampEx_Int_Goal",     # Sample extrapolation with Integration and Goal
                   "RL",                  # Reinforcement learning
                   "Random")              # Random choice

# model_lu: Shows the impression and choice rules for each model
model_lu <- tibble(
  model = c("SampEx_Heur_Goal", "SampEx_Heur_NoGoal", "SampEx_Int_Goal", "RL", "Random"),
  goal = c(100, Inf, 100, Inf, Inf),
  rule_Imp = c("SampExHeur", "SampExHeur", "SampExInt", "RL", "none"),
  rule_Choice = c("Softmax", "Softmax", "Softmax", "Softmax", "none")
)

# subj_fits contains all participants and models to be fit
subj_fits <- expand.grid(id = unique(dat$id)[1:min(c(subj_max, length(unique(dat$id))))],   # Reduce the number of participants for testing
                         model = models_to_fit, 
                         stringsAsFactors = FALSE)

# Combine subj_fits with model_lu

subj_fits <- subj_fits %>% 
  left_join(model_lu)


# mle_grid_fun
#   Takes a row number from subj_fits, and returns MLE estimates for the
#    subject and model in the given row
mle_grid_cluster_fun <- function(i) {
  
  # Get participant id
  id_i <- subj_fits$id[i]
  
  # Get subject data
  dat_subj <- dat %>% 
    filter(id == id_i)
  
  # Get model
  model_i <- subj_fits$model[i]
  
  # Get goal
  points_goal_i <- subj_fits$goal[i]
  
  # Get rule_Imp
  rule_Imp_i <- subj_fits$rule_Imp[i]
  
  # Get rule_Choice
  rule_Choice_i <- subj_fits$rule_Choice[i]
  
  if(model_i != "Random") {
  
  # Get impression and choice parameter combinations based on models
    
  # Impression parameter sequence
  if(grepl("RL", rule_Imp_i)) {pars_Imp_v <- alpha_par_v}  # RL impression uses alpha
  if(grepl("SampEx", rule_Imp_i)) {pars_Imp_v <- N_par_v}  # SampEx impression uses N
    
  # Choice parameter sequence
  if(grepl("Softmax", rule_Choice_i)) {pars_Choice_v <- phi_par_v} # Sofmax uses phi
    
  # Set up grid search for all combinations of parameters
  par_grid <- expand.grid(pars_Imp = pars_Imp_v,
                          pars_Choice = pars_Choice_v)
  
  # Loop over par_grid
  for(par_i in 1:nrow(par_grid)) {
    
    # Get parameters for current run
    pars_Imp_i <- par_grid$pars_Imp[par_i]
    pars_Choice_i <- par_grid$pars_Choice[par_i]
      
    fits_i <- Model_Lik(rule_Choice = rule_Choice_i,      # Choice rule
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
                        game_n = 10)                      # Number of games
  
    # Assign g2 to par_grid
    par_grid$dev[par_i] <- fits_i$deviance
    par_grid$g2[par_i] <- fits_i$g2
    
  }
  
  # Get minimum g2 value and parameters
  
  par_grid_min <- par_grid %>% 
    filter(g2 == min(g2)) %>%
    sample_n(1) # If there are more than 1 best combinations, choose one at random
  
  }
  
  if(model_i == "Random") {
    
    # Set g2 for a random model (and NA paramter values)
    par_grid_min <- data.frame(g2 = -2 * sum(log(rep(.5, 25 * 10))),
                              pars_Imp = NA,
                              pars_Choice = NA)
  }
  
  # Return best values
  return(c(id = id_i, 
           model = model_i, 
           g2 = round(par_grid_min$g2, 3),
           pars_Imp_mle = par_grid_min$pars_Imp,
           pars_Choice_mle = par_grid_min$pars_Choice))
  
}

# ---------------------------
# Set up and run cluster
# ----------------------------

# Set up cluster
cores_n <- 3   # Number of cores to run on
cl <- makeCluster(cores_n)

# Send libraries to cluster
clusterEvalQ(cl, library(tidyverse))

# Export objects to cluster
clusterExport(cl, list("subj_fits", 
                       "N_par_v", "phi_par_v", "alpha_par_v",
                       "trial_max", "Goal", "dat", 
                       "Model_Lik", 
                       "SampEx_Imp", "RL_Imp", 
                       "get_samples", 
                       "Softmax_Choice", 
                       "p_getthere"))

# Run cluster!
cluster_result_ls <- parallel::parLapply(cl = cl,
                                         fun = mle_grid_cluster_fun,
                                         X = 1:nrow(subj_fits))

stopCluster(cl)  # Stop cluster

# ---------------------------
# Organise cluster results
# ----------------------------

# Convert to dataframe
cluster_result_df <- as_tibble(do.call(what = rbind, 
                                       args = cluster_result_ls))

# Convert numeric values
cluster_result_df <- cluster_result_df %>%
  mutate(
    g2 = as.numeric(g2),
    pars_Imp_mle = as.numeric(pars_Imp_mle),
    pars_Choice_mle = as.numeric(pars_Choice_mle)
  )

# Combine subj_fits with cluster_result_df
subj_fits <- subj_fits %>%
  left_join(cluster_result_df)


# Get the best model for each subject
model_best <- subj_fits %>%
  group_by(id) %>%
  summarise(
    N_models = sum(g2 == min(g2)),       # Number of best fitting models (hopefully 1)
    model_best = model[g2 == min(g2)][1],
    model_best_g2 = g2[g2 == min(g2)][1],
    model_best_Imp = pars_Imp_mle[g2 == min(g2)][1],
    model_best_Choice = pars_Choice_mle[g2 == min(g2)][1]
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

# Table of results
table(model_best$model_best, model_best$goal.condition)







