rm(list = ls())
gc()


# Simulate choice patterns with different strategies and try and recover them

# Load libraries
library(tidyverse)

# Read learning functions
source("r/modeling_functions.R")

# to simulate data of all three environments run three times with the three
# different environments for varCond
subjects <- 350
games <- 10
trials <- 25
varCond <- "High"
set.seed(10)


# Note: Table of means and sds as used in the study

#           Option A      Option B
#         Mean    SD    Mean    SD
# Equal     4     2.5     4     11
# High     2.5    2.5     4     11
# Low       4     2.5   2.5     11
#

# Vectors with environment information

Envs <- c("Equal", "High", "Low")
sub_start_envs <- c(0, subjects, subjects * 2)
m_A_envs <- c(4, 2.5, 4)
sd_A_envs <- rep(2.5, 3)
m_B_envs <- c(4, 4, 2.5)
sd_B_envs <- rep(11, 3)



ind <- Envs == varCond
sub_start <- sub_start_envs[ind]
m_A <- m_A_envs[ind]
sd_A <- sd_A_envs[ind]

m_B <- m_B_envs[ind]
sd_B <- sd_B_envs[ind]

N_par_v <- 5:15                       # N paramter for SampEx Impression
alpha_par_v <- seq(0.1, 1, .01)        # Alpha parameter for reinforcement learning Impression
phi_par_v <- seq(0.1, 5, .05)         # Phi parameter for softmax choice
phi_par_v_SampEx <- seq(0.05, 1.5, 0.05)
curvature_par_v <- seq(0.2, 1, 0.05) # curvature parameter utility function RLGoal Impression
lambda_par_v <- seq(0.9, 2.5, 0.1)      # loss aversion parameter utility function RLGoal Impression
epsilon_par_v <- seq(0.60, 0.95, 0.05) # epsilon parameter for e-greedy
threshold_par_v <- seq(2, 20, 0.5)     # threshold for the ThreshHeur model

models_to_fit <- c("SampEx_Heur_Goal",    # Sample extrapolation with Heuristic and Goal
  #"SampEx_Heur_NoGoal",  # Sample extrapolation with Heuristic and NoGoal
  "GoalHeur",            # Goal Heuristic Model
  "ThreshHeur",          # Threshold Heuristic model
  "NaturalMean",         # Natural Mean Model
#  "SampEx_Int_Goal",     # Sample extrapolation with Integration and Goal
  "RL",                  # Reinforcement learning
  "RLGoal",              # Reinforcement learning target model (takes a goal into account)
  "Random")              # Random choice

n_models <- length(models_to_fit)


# create id variable
ids <- NULL

for (kk in (sub_start +1):(subjects + sub_start)){
  
  ids <- c(ids, paste0("id_", ifelse(kk < 10, "00", ifelse(kk < 100, "0", "")), kk))
  
}

dat <- tibble(
  id = rep(ids, each = trials * games), # 20 people per model
  game = rep(rep(1:games, each = trials), subjects), # 10 games
  trial = rep(rep(1:trials, games), subjects), # 25 trials per game
  model = rep(models_to_fit,
            each = subjects / n_models * games * trials),
  goal = rep(c(100, 100, 100, Inf, Inf, 100, Inf), each = subjects / n_models * games * trials),
  rule_Imp = rep(c("SampExHeur", "GoalHeur", "ThreshHeur", "Mean", "RL", "RLGoal", "none"),
                 each = subjects / n_models * games * trials),
  rule_Choice = rep(c("Softmax", "e-greedy", "e-greedy", "Softmax", "Softmax", "Softmax", "none"),
                    each = subjects / n_models * games * trials),
  outc_A = round(rnorm(trials * games * subjects, m_A, sd_A )),
  outc_B = round(rnorm(trials * games * subjects, m_B, sd_B )),
  pars_Imp = rep(c(sample(N_par_v, size = subjects / n_models, replace = TRUE),   # SampExHeur Model
                   rep(NA, subjects / n_models),                                  # GoalHeur Model
                   sample(threshold_par_v, size = subjects / n_models,
                          replace = TRUE),                                        # ThreshHeur Model
                   rep(NA, subjects / n_models),                                  # Mean Model
                   sample(alpha_par_v, size = subjects / n_models * 2,
                          replace = TRUE),                                        # RL and RLGoal Model
                   rep(NA, subjects / n_models)), each = games * trials),         # Random Model
  pars_Choice = rep(c(sample(phi_par_v_SampEx, size = subjects / n_models, replace = TRUE),
                      sample(epsilon_par_v, size = subjects / n_models * 2, replace = TRUE),
                      sample(phi_par_v, size = subjects / n_models, replace = TRUE),
                      sample(phi_par_v, size = subjects / n_models * 2, replace = TRUE),
                  rep(NA, subjects / n_models)), each = games * trials),
  selection = NA,
  outcome = NA,
  variance_condition = varCond
  
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


dat <- dat %>%
  group_by(id, game, trial, model, goal, rule_Imp, rule_Choice, outc_A, outc_B,
           pars_Imp, pars_Choice,
           variance_condition) %>%
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

# saveRDS(dat, paste0("data/Study1Data/useData/ModelSimDat_", varCond, ".rds"))
# 
# ## Run this when data files for each environments are available
# dat_Low <- readRDS("data/Study1Data/useData/ModelSimDat_Low.rds")
# dat_High <- readRDS("data/Study1Data/useData/ModelSimDat_High.rds")
# dat_Equal <- readRDS("data/Study1Data/useData/ModelSimDat_Equal.rds")
# 
# dat_all <- rbind(dat_Low, dat_High, dat_Equal)
# 
# saveRDS(dat_all, "data/Study1Data/useData/ModelSimDat_All.rds")

#------------------------
# Create some plots to see whether there are different patterns
# -----------------------

# 
# # Now make bins of data to get probability values
# get_bins <- function(id, model, point_vec, high_var_vec, nbins){
#   
#   # create bins
#   bin_size <- (max(point_vec) - min(point_vec)) / nbins
#   
#   bin_int <- cumsum(c(min(point_vec, na.rm = TRUE), rep(bin_size, nbins)))
#   
#   # compute the probability of choosing option 1, given a certain RSF.diff value
#   high_var_mean <- unlist(lapply(seq_len(nbins),
#                                  function(x, point_vec, bin_int, high_var_vec){
#                                    mean(high_var_vec[point_vec >= bin_int[x] &
#                                                        point_vec < bin_int[x+1]],
#                                         na.rm = TRUE)
#                                  },
#                                  point_vec = point_vec, bin_int = bin_int,
#                                  high_var_vec = high_var_vec))
#   
#   # compute the mean of the values from the bin
#   bin_mean <- unlist(lapply(seq_len(nbins),
#                             function(x, point_vec, bin_int, high_var_vec){
#                               mean(point_vec[point_vec >= bin_int[x] &
#                                                point_vec < bin_int[x+1]],
#                                    na.rm = TRUE)
#                             },
#                             point_vec = point_vec, bin_int = bin_int,
#                             high_var_vec = high_var_vec))
#   
#   # get the number of observations that were used in each bin
#   numobs <- unlist(lapply(seq_len(nbins),
#                           function(x, point_vec, bin_int, high_var_vec){
#                             vec <- high_var_vec[point_vec >= bin_int[x] &
#                                                   point_vec < bin_int[x+1]]
#                             vec <- vec[!is.na(vec)]
#                             
#                             lvec <- length(vec)
#                             
#                             lvec
#                             
#                           },
#                           point_vec = point_vec, bin_int = bin_int,
#                           high_var_vec = high_var_vec))
#   
#   # bind to data frame
#   df_bin <- tibble("id" = id,
#                    "pRisky" = high_var_mean,
#                    "mean_bin" = bin_mean,
#                    "nObs" = numobs,
#                    "model" = model
#   )
#   
# }
# 
# # define number of bins
# nbins <- 10
# 
# dat <- readRDS("data/Study1Data/useData/ModelSimDat_High.rds")
# 
# # first run to create data frame
# id <- unique(dat$id)[1]
# model <- dat$model[dat$id == id][1]
# point_vec <- dat$points.cum[dat$id == id]
# high_var_vec <- dat$high.var.chosen[dat$id == id]
# 
# bin_df <- get_bins(id, model, point_vec, high_var_vec, nbins)
# 
# 
# # Now go through the rest of the subjects and bind the data frames
# for (sub in 2:length(unique(dat$id))){
#   
#   id <- unique(dat$id)[sub]
#   model <- dat$model[dat$id == id][1]
#   point_vec <- dat$points.cum[dat$id == id]
#   high_var_vec <- dat$high.var.chosen[dat$id == id]
#   
#   bin_df_temp <- get_bins(id, model, point_vec, high_var_vec, nbins)
#   
#   bin_df <- rbind(bin_df, bin_df_temp)
#   
# }
# 
# library(yarrr)
# temp_col <- piratepal("basel")
# # plot the bin- and mean lines over all variance conditions
# cols <- c("Random" = temp_col[[1]],
#           "RL" = temp_col[[2]],
#           "ThreshHeur" = temp_col[[3]],
#           "GoalHeur" = temp_col[[4]])
# ggplot(bin_df, aes(x = mean_bin, y = pRisky)) + 
#   geom_line(data = filter(bin_df, model == "RL"),
#             aes(group=id), col = temp_col[[2]], lwd = .3, alpha = 0.08) +
#   geom_line(data = filter(bin_df, model == "Random"),
#             aes(group=id), col = temp_col[[1]], lwd = .3, alpha = 0.08) +
#   geom_line(data = filter(bin_df, model == "ThreshHeur"),
#             aes(group=id), col = temp_col[[3]], lwd = .3, alpha = 0.08) +
#   geom_line(data = filter(bin_df, model == "GoalHeur"),
#             aes(group=id), col = temp_col[[4]], lwd = .3, alpha = 0.08) +
#   stat_smooth(data = filter(bin_df, model == "Random"),
#               aes(col = "Random"), method ="loess", lwd = 1.5) +
#   stat_smooth(data = filter(bin_df, model == "RL"),
#               aes(col = "RL"), method = "loess", lwd = 1.5) +
#   stat_smooth(data = filter(bin_df, model == "GoalHeur"),
#               aes(col = "GoalHeur"), method = "loess", lwd = 1.5) +
#   stat_smooth(data = filter(bin_df, model == "ThreshHeur"),
#               aes(col = "ThreshHeur"), method = "loess", lwd = 1.5) +
#   scale_colour_manual(name = "Models", values = cols) +
# #  scale_fill_manual(name = "Models", values = cols) +
#   ylim(0,1) +
#   ylab("Likelihood Risky") + xlab("Points") +
#   theme_bw() +
#   theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
#   theme(axis.title.y = element_text(size = 15, vjust=0.3))
# 
# 
# # Create plot of pRisky by Trial
# df_trialAgg <- dat %>%
#   group_by(id, model, trial) %>%
#   summarise(risky_rate = mean(high.var.chosen))
# 
# 
# temp_col <- piratepal("basel")
# 
# cols <- c("Random" = temp_col[[1]],
#           "RL" = temp_col[[2]],
#           "ThreshHeur" = temp_col[[3]],
#           "GoalHeur" = temp_col[[4]])
# 
# ggplot(df_trialAgg, aes(x = trial, y = risky_rate)) + 
#   geom_line(data = filter(df_trialAgg, model == "Random"),
#             aes(group=id), col = temp_col[[1]], lwd = .3, alpha = 0.1) +
#   geom_line(data = filter(df_trialAgg, model == "RL"),
#             aes(group=id), col = temp_col[[2]], lwd = .3, alpha = 0.1) +
#   geom_line(data = filter(df_trialAgg, model == "ThreshHeur"),
#             aes(group=id), col = temp_col[[3]], lwd = .3, alpha = 0.1) +
#   geom_line(data = filter(df_trialAgg, model == "GoalHeur"),
#             aes(group=id), col = temp_col[[4]], lwd = .3, alpha = 0.1) +
#   stat_smooth(data = filter(df_trialAgg, model == "RL"),
#               aes(col = "RL"), method = "loess",  lwd = 1.5) +
#   stat_smooth(data = filter(df_trialAgg, model == "Random"),
#               aes(col = "Random"), method = "loess",  lwd = 1.5) +
#   stat_smooth(data = filter(df_trialAgg, model == "GoalHeur"),
#               aes(col = "GoalHeur"), method = "loess",  lwd = 1.5) +
#   stat_smooth(data = filter(df_trialAgg, model == "ThreshHeur"),
#               aes(col = "ThreshHeur"), method = "loess",  lwd = 1.5) +
#   scale_colour_manual(name="Models",values=cols) +
#   ylim(0,1) +
#   ylab("Likelihood Risky") + xlab("Trials") +
#   theme_bw() +
#   theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
#   theme(axis.title.y = element_text(size = 15, vjust=0.3))
