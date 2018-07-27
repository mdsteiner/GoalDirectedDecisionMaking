
# Clear working directory
rm(list = ls())
gc()

# Load libraries
library(dplyr)
library(parallel)

# SampExHeur Model fitting
source("R/modeling_functions.R")


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
subj_max <- length(unique(dat$id))


# Game parameters
trial_max <- 25
Goal <- 100

# ---------------------------
# Set up MLE fitting
# ----------------------------

# Grid search parameters
#   The grid search will look over all combinations of these parameters

N_par <- 15                      # N paramter for SampEx Impression
alpha_par_v <- seq(0.1, 0.9, .3)         # Alpha parameter for reinforcement learning Impression
phi_par_v <- seq(0.3, 2.7, .7)         # Phi parameter for softmax choice
epsilon_par_v <- seq(0.1, 0.9, .25)   # epsilon parameter for epsilon greedy choice rule
phi_par_v_SampEx <- seq(0.0, 1.5, .3)
threshold_par_v <- seq(0.0, 20, 2)

# Vector of all models to fit to each participant
models_to_fit <- c("SampEx_Heur_Goal",    # Sample extrapolation with Heuristic and Goal
  #"SampEx_Heur_NoGoal",  # Sample extrapolation with Heuristic and NoGoal
  "NaturalMean",         # Natural Mean Model
#  "SampEx_Int_Goal",     # Sample extrapolation with Integration and Goal
  "RL",                  # Reinforcement learning
  "RLGoal",              # Reinforcement learning target/ goal model
  "Random",              # Random choice
  "GoalHeur",            # Goal heuristic with e greedy
  "ThreshHeur")          # Threshold heuristic with e greedy

# model_lu: Shows the impression and choice rules for each model
model_lu <- tibble(
  model = c("SampEx_Heur_Goal", "NaturalMean", "RL", "RLGoal", "Random", "GoalHeur", "ThreshHeur"), #   "SampEx_Heur_NoGoal", "SampEx_Int_Goal"
  goal = c(100, Inf, Inf, 100, Inf, 100, 100), # 1100, 00, Inf
  rule_Imp = c("SampExHeur", "Mean", "RL", "RLGoal", "none", "GoalHeur", "ThreshHeur"), # "SampExHeur",  "SampExInt",
  rule_Choice = c("Softmax", "Softmax", "Softmax", "Softmax", "none", "e-greedy", "e-greedy") #   "Softmax", "Softmax",
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
mle_optim_cluster_fun <- function(i) {
  
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
    
    if(!grepl("SampEx", rule_Imp_i)){ # If the Model is not RLGoal it only needs one Impression parameter
      
      # Get impression and choice parameter combinations based on models
      
      # Impression parameter sequence
      if(grepl("RL", rule_Imp_i)) {
        
        startp_model <- as.matrix(expand.grid(phi_par_v,     # phi for updating rule 
                                               alpha_par_v))  # RL impression uses alpha
        
        low_model <- c(0, 0)
        up_model  <- c(Inf, 1)
        
      }
      
      if(grepl("RLGoal", rule_Imp_i)) {
        
        startp_model <- as.matrix(expand.grid(phi_par_v,        # phi for updating rule 
                                               alpha_par_v))    # RLGoal impression uses alpha updating
        
        low_model <- c(0, 0)
        up_model  <- c(Inf, 1)
        
      }
      
      if(grepl("Mean", rule_Imp_i)){
        
        startp_model <- as.matrix(expand.grid(phi_par_v))       # phi for updating rule 
        
        low_model <- c(0)
        up_model  <- c(Inf)
        
      }
      
      if(grepl("GoalHeur", rule_Imp_i)) {
        
        startp_model <- as.matrix(expand.grid(epsilon_par_v))  # epsilon for updating rule 
        
        low_model <- c(0)
        up_model  <- c(1)
        
      }
      
      if(grepl("ThreshHeur", rule_Imp_i)) {
        
        startp_model <- as.matrix(expand.grid(epsilon_par_v,   # epsilon for updating rule 
                                              threshold_par_v))
        
        low_model <- c(0, 0)
        up_model  <- c(1, Inf)
        
      }
      
      
        
        
      grid_model <- apply(startp_model, 1, Model_Lik_Optim,
                          rule_Choice = rule_Choice_i,
                          rule_Imp = rule_Imp_i,
                          mem_N = NA,
                          selection_v = dat_subj$selection,
                          outcome_v = dat_subj$outcome,
                          trial_v = dat_subj$trial,
                          game_v = dat_subj$game,
                          trial_max = 25,
                          points_goal = points_goal_i,
                          option_n = 2,
                          game_n = 10)
      
      
      
      
      fit_model <- optim(startp_model[which.min(grid_model),],
                         Model_Lik_Optim,
                         method = "L-BFGS-B",
                         lower = low_model,
                         upper = up_model,
                         rule_Choice = rule_Choice_i,
                         rule_Imp = rule_Imp_i,
                         mem_N = NA,
                         selection_v = dat_subj$selection,
                         outcome_v = dat_subj$outcome,
                         trial_v = dat_subj$trial,
                         game_v = dat_subj$game,
                         trial_max = 25,
                         points_goal = points_goal_i,
                         option_n = 2,
                         game_n = 10)
       
       
         deviance_model <- fit_model$val
         pars_model <- fit_model$par
       
         pred_model <- mean(Model_Lik_Optim(fit_model$par,
                                            rule_Choice = rule_Choice_i,
                                            rule_Imp = rule_Imp_i,
                                            mem_N = NA,
                                            selection_v = dat_subj$selection,
                                            outcome_v = dat_subj$outcome,
                                            trial_v = dat_subj$trial,
                                            game_v = dat_subj$game,
                                            trial_max = 25,
                                            points_goal = points_goal_i,
                                            option_n = 2,
                                            game_n = 10,
                                            prediction = TRUE))
         
         
         if(grepl("RL", rule_Imp_i)) {
           
           pars_Imp <- pars_model[2]
           pars_Choice <- pars_model[1]
           bic <- deviance_model + 2 * log(length(dat_subj$selection))
           
         }
         
         if(grepl("RLGoal", rule_Imp_i)) {
           
           pars_Imp <- pars_model[2]
           pars_Choice <- pars_model[1]
           bic <- deviance_model + 2 * log(length(dat_subj$selection))
           
         }
         
         if(grepl("Mean", rule_Imp_i)){
           
           pars_Imp <- NA
           pars_Choice <- pars_model[1]
           bic <- deviance_model + 1 * log(length(dat_subj$selection))
           
         }
         
         if(grepl("GoalHeur", rule_Imp_i)){
           
           pars_Imp <- NA
           pars_Choice <- pars_model[1]
           bic <- deviance_model + 1 * log(length(dat_subj$selection))
           
         }
         
         if(grepl("ThreshHeur", rule_Imp_i)){
           
           pars_Imp <- pars_model[2]
           pars_Choice <- pars_model[1]
           bic <- deviance_model + 2 * log(length(dat_subj$selection))
           
         }
      
      
    } else if(grepl("SampEx", rule_Imp_i)){ # If the model is SampEx, it's a bit more complicated because of the discrete parameter
      
      # Get impression and choice parameter combinations based on models
      
      startp_model <- as.matrix(expand.grid(phi_par_v_SampEx))       # phi for updating rule 
      
      low_model <- c(0)
      up_model  <- c(Inf)
      
      temp_fit_SampEx <- Inf
      
      # Loop over par_grid
      for(par_i in 1:N_par) {
        
        grid_model <- apply(startp_model, 1, Model_Lik_Optim,
                            rule_Choice = rule_Choice_i,
                            rule_Imp = rule_Imp_i,
                            mem_N = par_i,
                            selection_v = dat_subj$selection,
                            outcome_v = dat_subj$outcome,
                            trial_v = dat_subj$trial,
                            game_v = dat_subj$game,
                            trial_max = 25,
                            points_goal = points_goal_i,
                            option_n = 2,
                            game_n = 10)
        
        
        
        
        tfit_model <- optim(startp_model[which.min(grid_model),],
                           Model_Lik_Optim,
                           method = "L-BFGS-B",
                           lower = low_model,
                           upper = up_model,
                           rule_Choice = rule_Choice_i,
                           rule_Imp = rule_Imp_i,
                           mem_N = par_i,
                           selection_v = dat_subj$selection,
                           outcome_v = dat_subj$outcome,
                           trial_v = dat_subj$trial,
                           game_v = dat_subj$game,
                           trial_max = 25,
                           points_goal = points_goal_i,
                           option_n = 2,
                           game_n = 10)
        
        if(tfit_model$val < temp_fit_SampEx){
                 temp_fit_SampEx <- tfit_model$val
                 fit_model <- tfit_model
                 which_N <- par_i
               }
        
      }
      
      deviance_model <- fit_model$val
      pars_model <- fit_model$par
      
      pred_model <- mean(Model_Lik_Optim(fit_model$par,
                                         rule_Choice = rule_Choice_i,
                                         rule_Imp = rule_Imp_i,
                                         mem_N = which_N,
                                         selection_v = dat_subj$selection,
                                         outcome_v = dat_subj$outcome,
                                         trial_v = dat_subj$trial,
                                         game_v = dat_subj$game,
                                         trial_max = 25,
                                         points_goal = points_goal_i,
                                         option_n = 2,
                                         game_n = 10,
                                         prediction = TRUE))
      
      pars_Imp <- which_N
      pars_Choice <- pars_model[1]
      bic <- deviance_model + 2 * log(length(dat_subj$selection))
      
    }
    
    
  }
  
  if(model_i == "Random") {
    
    # Set bic for a random model (and NA paramter values)
    deviance_model <- -2 * sum(log(rep(.5, 25 * 10)))
    bic <- deviance_model

    pred_model <- mean(sample(1:2, 250, replace = TRUE) == dat_subj$selection)
    
    pars_Imp <- NA
    pars_Choice <- NA
  }
  
    
  # Return best values
  return(c(id = id_i, 
           model = model_i, 
           bic = round(bic, 3),
           pars_Imp_mle = pars_Imp,
           pars_Choice_mle = unname(unlist(pars_Choice)),
           pred_mle = pred_model))
  
}

# ---------------------------
# Set up and run cluster
# ----------------------------

# Set up cluster
cores_n <- detectCores()  # Number of cores to run on
cl <- makeCluster(cores_n)

# Send libraries to cluster
clusterEvalQ(cl, library(dplyr))

# Export objects to cluster
clusterExport(cl, list("subj_fits", 
                       "N_par", "phi_par_v", "alpha_par_v", "epsilon_par_v", "phi_par_v_SampEx", "threshold_par_v",
                       "trial_max", "Goal", "dat", 
                       "Model_Lik_Optim", 
                       "SampEx_Imp", "RL_Imp", "RLGoal_Imp", "Mean_Imp", "GoalHeur", "Thresh_Imp",
                       "get_samples", "uncor_var", 
                       "Softmax_Choice", 
                       "p_getthere"))

# Run cluster!
cluster_result_ls <- parallel::parLapply(cl = cl,
                                         fun = mle_optim_cluster_fun,
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
    pars_Choice_mle = as.numeric(pars_Choice_mle),
    pred_mle = as.numeric(pred_mle)
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
    model_best_Choice = pars_Choice_mle[bic == min(bic)][1],
    model_best_pred = pred_mle[bic == min(bic)][1]
  )


if (modelRecovery){
  
  model_best <- dat %>%
    group_by(id) %>%
    summarise(
      true_Model = model[1],
      true_Pars_Imp = pars_Imp[1],
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

save.image("ModelFitOptim04.RData")
Sys.time()

