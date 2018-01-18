rm(list = ls())
gc()
# RLHeur Model fitting

source("R/ReinforcementLearningModel.R")

# get data

dat <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")

dat$game <- dat$game - 1
dat$selection <- as.numeric(as.character(dat$selection))
dat$selection <- dat$selection - 1

dat <- subset(dat, game > 0)

N_sub <- length(unique(dat$id))

N_trial <- 25
N_game <- 10

Goal <- 100



low_RL <- c(0, 0.00001)
up_RL  <- c(1, Inf)
r_parr_RL <- matrix(NA, ncol = 2, nrow = N_sub)
r_good_RL <- c()
r_pred_RL <- c()



fitruns <- 4
Nstart <- 400

for (ii in seq_len(N_sub)){

  temp_fit_RL <- Inf
  
  fit_ind <- dat$id == unique(dat$id)[ii]
  v_As_temp <- ifelse(dat$selection[fit_ind] == 0, dat$outcome[fit_ind], NA)
  v_Bs_temp <- ifelse(dat$selection[fit_ind] == 1, dat$outcome[fit_ind], NA)

  for(ww in 1:fitruns){
    startp_RL <- t(replicate(Nstart,
                                 c(runif(1, 0.05, 0.95), runif(1, 0.1, 100))))


    grid_RL <- apply(startp_RL, 1, RL_Fit, chd = dat$selection[fit_ind],
                     v_As = v_As_temp, v_Bs = v_Bs_temp,
                     exp_prior_start = c(0, 0), n_trials = N_trial,
                     n_games = N_game)


    tfit_RL <- optim(startp_RL[which.min(grid_RL),], RL_Fit,
                     method = "L-BFGS-B", lower = low_RL,
                     upper = up_RL, chd = dat$selection[fit_ind],
                     v_As = v_As_temp, v_Bs = v_Bs_temp,
                     exp_prior_start = c(0, 0), n_trials = N_trial,
                     n_games = N_game)
    
    if(tfit_RL$val < temp_fit_RL){
      temp_fit_RL <- tfit_RL$val
      fit_RL <- tfit_RL
    }
  }


  r_good_RL[ii] <- fit_RL$val
  r_parr_RL[ii,] <- fit_RL$par

  r_pred_RL[ii] <- mean(RL_Fit(fit_RL$par,chd = dat$selection[fit_ind],
                               v_As = v_As_temp, v_Bs = v_Bs_temp,
                               exp_prior_start = c(0, 0), n_trials = N_trial,
                               n_games = N_game, pred=TRUE))

  if (ii %in% c(seq(10, 400, 10))) {
    print(ii)
  }

}

med_par_RL <- apply(r_parr_RL, 2, median)

hist(r_pred_RL)

save.image("data/Study1Data/useData/RL_Fits.RData")



# N_par <- 1:20
# phi_par <- seq(0.01, 10, .6)
# 
# startp_RL <- as.matrix(expand.grid(N_par, phi_par))
# 
# nrow(startp_RL)
# 
# for (ii in seq_len(N_sub)){
#   
#   temp_fit_RL <- Inf
#   
#   
#   fit_ind <- dat$id == dat$id[ii]
#   v_As_temp <- ifelse(dat$selection[fit_ind] == 0, dat$outcome[fit_ind], NA)
#   v_Bs_temp <- ifelse(dat$selection[fit_ind] == 1, dat$outcome[fit_ind], NA)
#   
#   
#   grid_RL <- apply(startp_RL, 1, RL_Fit, chd = dat$selection[fit_ind],
#                    v_As = v_As_temp, v_Bs = v_Bs_temp, T = N_trial,
#                    curr_t = dat$trial[fit_ind], goal = Goal,
#                    curr_Y = dat$points.cum[fit_ind])
#   
#   
#   
#   
#   r_good_RL[ii] <- RL_Fit(startp_RL[which.min(grid_RL),],
#                           chd = dat$selection[fit_ind],
#                           v_As = v_As_temp, v_Bs = v_Bs_temp,
#                           T = N_trial, curr_t = dat$trial[fit_ind],
#                           goal = Goal, curr_Y = dat$points.cum[fit_ind])
#   r_parr_RL[ii,] <- startp_RL[which.min(grid_RL),]
#   
#   r_pred_RL[ii] <- mean(RL_Fit(startp_RL[which.min(grid_RL),],
#                                chd = dat$selection[fit_ind],
#                                v_As = v_As_temp, v_Bs = v_Bs_temp,
#                                T = N_trial, curr_t = dat$trial[fit_ind],
#                                goal = Goal, curr_Y = dat$points.cum[fit_ind],
#                                pred=TRUE))
#   
#   print(ii)
#   
# }
# 
# med_par_RL <- apply(r_parr_RL, 2, median)
# 
# hist(r_pred_RL)
# 
# save.image("data/Study1Data/useData/RL_Fits.RData")