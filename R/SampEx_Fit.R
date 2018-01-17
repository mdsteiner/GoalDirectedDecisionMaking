rm(list = ls())
gc()
# SampExHeur Model fitting

source("R/SampExHeurModel.R")

# get data

dat <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")

dat$game <- dat$game - 1
dat$selection <- as.numeric(as.character(dat$selection))
dat$selection <- dat$selection - 1

dat <- subset(dat, game > 0)

N_sub <- length(unique(dat$id))

N_trial <- 25

Goal <- 100

low_SampEx <- c(1, 0.0001)
up_SampEx  <- c(15, Inf)
r_parr_SampEx <- matrix(NA, ncol = 2, nrow = N_sub)
r_good_SampEx <- c()
r_pred_SampEx <- c()



N_par <- 1:20
phi_par <- seq(0.0, 25, .5)

startp_SampEx <- as.matrix(expand.grid(N_par, phi_par))

nrow(startp_SampEx)

for (ii in seq_len(N_sub)){
  
  temp_fit_SampEx <- Inf
  
    
  fit_ind <- dat$id == unique(dat$id)[ii]
  v_As_temp <- ifelse(dat$selection[fit_ind] == 0, dat$outcome[fit_ind], NA)
  v_Bs_temp <- ifelse(dat$selection[fit_ind] == 1, dat$outcome[fit_ind], NA)
    
    
  grid_SampEx <- apply(startp_SampEx, 1, SampExFit, chd = dat$selection[fit_ind],
                      v_As = v_As_temp, v_Bs = v_Bs_temp, T = N_trial,
                      curr_t = dat$trial[fit_ind], goal = Goal,
                      curr_Y = dat$points.cum[fit_ind])
    
    
  
  
  r_good_SampEx[ii] <- SampExFit(startp_SampEx[which.min(grid_SampEx),],
                                 chd = dat$selection[fit_ind],
                                 v_As = v_As_temp, v_Bs = v_Bs_temp,
                                 T = N_trial, curr_t = dat$trial[fit_ind],
                                 goal = Goal, curr_Y = dat$points.cum[fit_ind])
  r_parr_SampEx[ii,] <- startp_SampEx[which.min(grid_SampEx),]
  
  r_pred_SampEx[ii] <- mean(SampExFit(startp_SampEx[which.min(grid_SampEx),],
                                      chd = dat$selection[fit_ind],
                                      v_As = v_As_temp, v_Bs = v_Bs_temp,
                                      T = N_trial, curr_t = dat$trial[fit_ind],
                                      goal = Goal, curr_Y = dat$points.cum[fit_ind],
                                      pred=TRUE))
  
  print(ii)
  
}

med_par_SampEx <- apply(r_parr_SampEx, 2, median)

hist(r_pred_SampEx)

save.image("data/Study1Data/useData/SampExHeur_Fits.RData")








# 
# fitruns <- 2
# Nstart <- 400
# 
# for (ii in seq_len(N_sub)){
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
