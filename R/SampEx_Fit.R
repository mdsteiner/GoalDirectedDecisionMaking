
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
subjects_N <- length(unique(dat$id))

# ---------------------------
# Set up MLE fitting
# ----------------------------

# Grid search parameters
#   The grid search will look over all combinations of these parameters
N_par_v <- 1:15
phi_par_v <- seq(0.0, .15, .01)
models_to_fit <- c("SampExGoal", "SampExNoGoal", "Random")

# Maximum number of subjects to fit (for testing)
subj_max <- 50  # length(unique(dat$id))

# subj_fits contains all participants and models to be fit
subj_fits <- expand.grid(id = unique(dat$id)[1:subj_max],   # Reduce the number of participants for testing
                         model = models_to_fit, 
                         stringsAsFactors = FALSE)

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
  
  if(model_i != "Random") {
  
  # Define goal based on model
  points_goal_i <- ifelse(model_i == "SampExGoal", 
                          100, 
                          Inf)
  
  # Set up grid search
  par_grid <- expand.grid(N = N_par_v,
                          phi = phi_par_v)
  
  # Loop over par_grid
  for(par_i in 1:nrow(par_grid)) {
    
    # Get parameters for current run
    N_i <- par_grid$N[par_i]
    phi_i <- par_grid$phi[par_i]
    
    # Fitting values for SampExGoal and SampExNoGoal models
    if(model_i %in% c("SampExGoal", "SampExNoGoal")) {
      
    fits_i <- SampEx_Lik(pars = c(N_i, phi_i),
                         selection_v = dat_subj$selection,    
                         outcome_v = dat_subj$outcome,
                         trial_v = dat_subj$trial,         # trial_v: Vector of trial numbers
                         game_v = dat_subj$game,           # game_v: Vector of game numbers
                         trial_max = 25,                   # trial_max: Maximum number of trials in task
                         points_goal = points_goal_i,      # points_goal: Points desired at goal. If Infinite, then impressions is based on mean
                         option_n = 2, # option_n: Number of options
                         game_n = 10)
    
    }
    
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
    par_grid_min = data.frame(g2 = -2 * sum(log(rep(.5, trial_max * 10))),
                              N_mle = NA,
                              phi_mle = NA)
  }
  
  # Return best values
  return(c(id = id_i, 
           model = model_i, 
           g2 = round(par_grid_min$g2, 3),
           N_mle = par_grid_min$N,
           phi_mle =  par_grid_min$phi))
  
}

# ---------------------------
# Set up and run cluster
# ----------------------------

# Set up cluster
cores_n <- 7   # Number of cores to run on
cl <- makeCluster(cores_n)

# Send libraries to cluster
clusterEvalQ(cl, library(tidyverse))

# Export objects to cluster
clusterExport(cl, list("subj_fits", "N_par_v", "phi_par_v", 
                       "trial_max", "Goal", "dat", "SampEx_Lik", 
                       "SampEx_Imp", "get_samples", "softmax_Choice"))

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
    N_mle = as.numeric(N_mle),
    phi_mle = as.numeric(phi_mle)
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
    model_best_N = N_mle[g2 == min(g2)][1],
    model_best_phi = phi_mle[g2 == min(g2)][1]
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








# 
# fitruns <- 2
# Nstart <- 400
# 
# for (ii in seq_len(subjects_N)){
# 
#   temp_fit_SampEx <- Inf
# 
#   for(ww in 1:fitruns){
#     startp_SampEx <- t(replicate(Nstart,
#                                  c(round(runif(1, 0, 14)), runif(1, 0.0001, 2))))
# 
#     fit_ind <- dat$id == dat$id[ii]
#     v_As_temp <- ifelse(dat$selection[fit_ind] == 0, dat$outcome[fit_ind], NA)
#     v_Bs_temp <- ifelse(dat$selection[fit_ind] == 1, dat$outcome[fit_ind], NA)
# 
# 
#     grid_SampEx <- apply(startp_SampEx, 1, SampExFit, chd = dat$selection[fit_ind],
#                          v_As = v_As_temp, v_Bs = v_Bs_temp, T = N_trial,
#                          curr_t = dat$trial[fit_ind], goal = Goal,
#                          curr_Y = dat$points.cum[fit_ind])
# 
# 
#     tfit_SampEx <- optim(startp_SampEx[which.min(grid_SampEx),], SampExFit,
#                          method = "L-BFGS-B", lower = low_SampEx,
#                          upper = up_SampEx, chd = dat$selection[fit_ind],
#                          v_As = v_As_temp, v_Bs = v_Bs_temp, T = N_trial,
#                          curr_t = dat$trial[fit_ind], goal = Goal,
#                          curr_Y = dat$points.cum[fit_ind])
#     if(tfit_SampEx$val < temp_fit_SampEx){
#       temp_fit_SampEx <- tfit_SampEx$val
#       fit_SampEx <- tfit_SampEx
#     }
#   }
# 
# 
#   r_good_SampEx[ii] <- fit_SampEx$val
#   r_parr_SampEx[ii,] <- fit_SampEx$par
# 
#   r_pred_SampEx[ii] <- mean(SampExFit(fit_SampEx$par,chd = dat$selection[fit_ind],
#                                       v_As = v_As_temp, v_Bs = v_Bs_temp,
#                                       T = N_trial, curr_t = dat$trial[fit_ind],
#                                       goal = Goal, curr_Y = dat$points.cum[fit_ind],
#                                       pred=TRUE))
# 
# 
# 
# }
# 
# med_par_SampEx <- apply(r_parr_SampEx, 2, median)
# 
# hist(r_pred_SampEx)
# 
# save.image("data/Study1Data/useData/SampExHeur_Fits.RData")
