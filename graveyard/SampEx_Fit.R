
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

modelRecovery <- FALSE

if (modelRecovery){
  
  dat <- as_tibble(readRDS("data/Study1Data/useData/ModelSimDat_All.rds"))
  
  # Data cleaning
  dat <- dat %>%
    mutate(
      selection = as.numeric(as.character(selection))
    ) %>%
    filter(
      game > 0   # Remove game == 0 (practice games)
    ) 
  
} else {
  # get trial level data
  dat <- as_tibble(readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds"))
  
  # Data cleaning
  dat <- dat %>%
    mutate(
      game = game - 1,
      selection = as.numeric(as.character(selection))
    ) %>%
    filter(
      game > 0   # Remove game == 0 (practice games)
    ) 
}



# Number of Subjects
subj_max <- 4 #length(unique(dat$id))


# Game parameters
trial_max <- 25
Goal <- 100

# ---------------------------
# Set up MLE fitting
# ----------------------------

# Grid search parameters
#   The grid search will look over all combinations of these parameters

N_par_v <- 1:15                      # N paramter for SampEx Impression
alpha_par_v <- seq(0, 1, .1)         # Alpha parameter for reinforcement learning Impression
phi_par_v <- seq(0.0, 3, .3)         # Phi parameter for softmax choice
phi_par_v_SampEx <- seq(0.0, 1.5, .15)
curvature_par_v <- seq(0.0, 1, 0.25)  # curvature parameter utility function RLGoal Impression
lambda_par_v <- c(1, 1.5, 2.25) # loss aversion parameter utility function RLGoal Impression

# Vector of all models to fit to each participant
models_to_fit <- c(#"SampEx_Heur_Goal",    # Sample extrapolation with Heuristic and Goal
                   #"SampEx_Heur_NoGoal",  # Sample extrapolation with Heuristic and NoGoal
                   "NaturalMean",         # Natural Mean Model
                   "SampEx_Int_Goal",     # Sample extrapolation with Integration and Goal
                   "RL",                  # Reinforcement learning
                   "RLGoal",              # Reinforcement learning target/ goal model
                   "Random")              # Random choice

# model_lu: Shows the impression and choice rules for each model
model_lu <- tibble(
  model = c("NaturalMean", "SampEx_Int_Goal", "RL", "RLGoal", "Random"), # "SampEx_Heur_Goal", "SampEx_Heur_NoGoal"
  goal = c(Inf, 100, Inf, 100, Inf), # 100, Inf
  rule_Imp = c("Mean", "SampExInt", "RL", "RLGoal", "none"), #"SampExHeur", "SampExHeur"
  rule_Choice = c("Softmax", "Softmax", "Softmax", "Softmax", "none") # "Softmax", "Softmax"
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
  
  if(!grepl("RLGoal", rule_Imp_i)){ # If the Model is not RLGoal it only needs one Impression parameter
      
  # Get impression and choice parameter combinations based on models
  
  # Impression parameter sequence
  if(grepl("RL", rule_Imp_i)) {pars_Imp_v <- alpha_par_v}  # RL impression uses alpha
  
  if(grepl("Mean", rule_Imp_i)){pars_Imp_v <- NA} # No Impression parameter for the mean rule
    
  # Choice parameter sequence
  if(grepl("Softmax", rule_Choice_i)) {pars_Choice_v <- phi_par_v} # Sofmax uses phi
    
  if(grepl("SampEx", rule_Imp_i)) {
    pars_Imp_v <- N_par_v    # SampEx impression uses N
    pars_Choice_v <- phi_par_v_SampEx
    } 
    
  # Set up grid search for all combinations of parameters
  par_grid <- expand.grid(pars_Imp = pars_Imp_v,
                          pars_Choice = pars_Choice_v,
                          pars_Imp_curvature = NA,
                          pars_Imp_lambda = NA)
  
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
  
    # Assign bic to par_grid
    par_grid$dev[par_i] <- fits_i$deviance
    par_grid$bic[par_i] <- fits_i$bic
    
  }
  
  } else if(grepl("RLGoal", rule_Imp_i)){ # If the model is RLGoal it needs three Impression parameters
    
    # Get impression and choice parameter combinations based on models
    
    # Impression parameter sequence
    pars_Imp_v_alpha <- alpha_par_v  # RL impression uses alpha
    pars_Imp_v_curvature <- curvature_par_v  # RL impression uses alpha
    pars_Imp_v_lambda <- lambda_par_v  # RL impression uses alpha
    
    
    # Choice parameter sequence
    if(grepl("Softmax", rule_Choice_i)) {pars_Choice_v <- phi_par_v} # Sofmax uses phi
    
    # Set up grid search for all combinations of parameters
    par_grid <- expand.grid(pars_Imp = pars_Imp_v_alpha,
                            pars_Imp_curvature = pars_Imp_v_curvature,
                            pars_Imp_lambda = pars_Imp_v_lambda,
                            pars_Choice = pars_Choice_v)
    
    # Loop over par_grid
    for(par_i in 1:nrow(par_grid)) {
      
      # Get parameters for current run
      pars_Imp_i <- c(par_grid$pars_Imp[par_i],
                      par_grid$pars_Imp_curvature[par_i],
                      par_grid$pars_Imp_lambda[par_i])
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
      
      # Assign bic to par_grid
      par_grid$dev[par_i] <- fits_i$deviance
      par_grid$bic[par_i] <- fits_i$bic
      
    }
    
  }
  
  # Get minimum bic value and parameters
  
  par_grid_min <- par_grid %>% 
    filter(bic == min(bic, na.rm = TRUE)) %>%
    sample_n(1) # If there are more than 1 best combinations, choose one at random
  
  }
  
  if(model_i == "Random") {
    
    # Set bic for a random model (and NA paramter values)
    par_grid_min <- data.frame(bic = -2 * sum(log(rep(.5, 25 * 10))),
                              pars_Imp = NA,
                              pars_Choice = NA,
                              pars_Imp_curvature = NA,
                              pars_Imp_lambda = NA)
  }
  
  # Return best values
  return(c(id = id_i, 
           model = model_i, 
           bic = round(par_grid_min$bic, 3),
           pars_Imp_mle = par_grid_min$pars_Imp,
           pars_Imp_curvature_mle = par_grid_min$pars_Imp_curvature,
           pars_Imp_lambda_mle = par_grid_min$pars_Imp_lambda,
           pars_Choice_mle = par_grid_min$pars_Choice))
  
}

# ---------------------------
# Set up and run cluster
# ----------------------------

# Set up cluster
cores_n <- 4   # Number of cores to run on
cl <- makeCluster(cores_n)

# Send libraries to cluster
clusterEvalQ(cl, library(tidyverse))

# Export objects to cluster
clusterExport(cl, list("subj_fits", 
                       "N_par_v", "phi_par_v", "alpha_par_v", "curvature_par_v", "lambda_par_v", "phi_par_v_SampEx",
                       "trial_max", "Goal", "dat", 
                       "Model_Lik", 
                       "SampEx_Imp", "RL_Imp", "RLGoal_Imp", "Mean_Imp",
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
    bic = as.numeric(bic),
    pars_Imp_mle = as.numeric(pars_Imp_mle),
    pars_Imp_curvature_mle = as.numeric(pars_Imp_curvature_mle),
    pars_Imp_lambda_mle = as.numeric(pars_Imp_lambda_mle),
    pars_Choice_mle = as.numeric(pars_Choice_mle)
  )

# Combine subj_fits with cluster_result_df
subj_fits <- subj_fits %>%
  left_join(cluster_result_df)


# Get the best model for each subject
model_best <- subj_fits %>%
  group_by(id) %>%
  summarise(
    N_models = sum(bic == min(bic)),       # Number of best fitting models (hopefully 1)
    model_best = model[bic == min(bic)][1],
    model_best_bic = bic[bic == min(bic)][1],
    model_best_Imp = pars_Imp_mle[bic == min(bic)][1],
    model_best_Imp_curvature = pars_Imp_curvature_mle[bic == min(bic)][1],
    model_best_Imp_lambda = pars_Imp_lambda_mle[bic == min(bic)][1],
    model_best_Choice = pars_Choice_mle[bic == min(bic)][1]
  )


if (modelRecovery){
  
  model_best <- dat %>%
    group_by(id) %>%
    summarise(
      true_Model = model[1],
      true_Pars_Imp = pars_Imp[1],
      true_Pars_Imp_curvature = pars_Imp_curvature[1],
      true_Pars_Imp_lambda = pars_Imp_lambda[1],
      true_Pars_Choice = pars_Choice[1]
    ) %>% 
    ungroup() %>%
    left_join(model_best)   # Add modelling results
  
  prop.table(table(model_best$model_best))
  mean(model_best$model_best == model_best$true_Model)
  
} else {
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

}



Sys.time()

