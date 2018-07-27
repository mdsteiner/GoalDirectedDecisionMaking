# bayesian rl model fitting

rm(list = ls())

# Load libraries
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load trial level data
model_recovery <- TRUE

if(model_recovery){
  
  dat <- as_tibble(readRDS("data/Study1Data/useData/ModelSimDat_All.rds"))
  
  # subset for tryout
  dat <- dat[dat$id %in% unique(dat$id[dat$model == "RL"])[1:30],]
  
  
} else {
  
  dat <- as_tibble(readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds"))
  
  # subset for tryout
  dat <- dat[dat$id %in% unique(dat$id)[1:30],]
  
  dat <- dat %>%
    filter(game > 1) %>%
    mutate(game = game - 1)
  
}


# prepare data in right format

ids <- unique(dat$id)
N <- length(ids)
n_trials <- 25
n_games <- 10
outcomes <- matrix(NA, ncol = N, nrow = n_games * n_trials)
choices <- matrix(NA, ncol = N, nrow = n_games * n_trials)

for (subj in seq_len(N)){
  
  outcomes[, subj] <- dat$outcome[dat$id == ids[subj]]
  choices[, subj] <- as.numeric(as.character(dat$selection[dat$id == ids[subj]])) - 1
  
}

data_list <- list(N, n_games, n_trials, outcomes, choices)

fit <- stan(file = 'stan/rl_model.stan', data = data_list, 
            iter = 3000, chains = 4)

print(fit)
plot(fit)

if (model_recovery){
  
  dn <- dat %>%
    group_by(id) %>%
    summarise(
      pars_Imp = pars_Imp[1],
      pars_Choice = pars_Choice[1]
    )
  
  fit_df <- as.data.frame(fit)
  plot(colMeans(fit_df[,1:30]), dn$pars_Imp,
       xlab = "stan fit", ylab = "optim fit", main = "alpha parameters",
       ylim = c(0, 1), xlim = c(0, 1), pch = 16)
  abline(0, 1)
  
  plot(colMeans(fit_df[,31:60]), dn$pars_Choice,
       xlab = "stan fit", ylab = "optim fit", main = "phi parameters",
       ylim = c(0, 1), xlim = c(0, 15), pch = 16)
  abline(0, 1)
  
}




