# simple rl simulation

rm(list = ls())

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

N <- 1
alpha <- .3
phi <- .5
n_trials <- 25
n_games <- 20

outcomes <- matrix(c(rnorm(n_games * n_trials, 5, 2), rnorm(n_games * n_trials, 6, 2)), byrow = FALSE,
                   ncol = 2)
chosen_outcomes <- rep(NA, n_games * n_trials)

selection <- rep(NA, n_games * n_trials)
impressions <- matrix(NA, ncol = 2, nrow = n_games * n_trials)
sel_probs <- matrix(NA, ncol = 2, nrow = n_games * n_trials)


for (kk in seq_len(n_games)){
  
  impressions[(kk - 1) * n_trials + 1,] <- 0
  selection[(kk - 1) * n_trials + 1] <- sample(c(0, 1), 1)
  sel_probs[(kk - 1) * n_trials + 1,] <- 0.5
  chosen_outcomes[(kk - 1) * n_trials + 1] <- outcomes[1, selection[(kk - 1) * n_trials + 1] + 1]
  
  
  for (i in 2:n_trials){
    
    impressions[(kk - 1) * n_trials + i, selection[(kk - 1) * n_trials + i - 1] + 1] <-
      (1 - alpha) * impressions[(kk - 1) * n_trials + i - 1, selection[(kk - 1) *
      n_trials + i - 1] + 1] + alpha * outcomes[(kk - 1) * n_trials + i - 1]
    
    impressions[(kk - 1) * n_trials + i, is.na(impressions[(kk - 1) * n_trials + i,])] <-
      impressions[(kk - 1) * n_trials + i - 1, is.na(impressions[(kk - 1) * n_trials + i,])]
    
    sel_probs[(kk - 1) * n_trials + i,] <- exp( phi * impressions[(kk - 1) * n_trials + i,]) /
      sum(exp(phi * impressions[(kk - 1) * n_trials + i,]))
    
    sel_probs[(kk - 1) * n_trials + i, sel_probs[(kk - 1) * n_trials + i,] > .999] <- .999
    sel_probs[(kk - 1) * n_trials + i, sel_probs[(kk - 1) * n_trials + i,] < .001] <- .001
    
    selection[(kk - 1) * n_trials + i] <- sample(c(0, 1), 1, prob = sel_probs[(kk - 1) * n_trials + i,])
    chosen_outcomes[(kk - 1) * n_trials + i] <- outcomes[(kk - 1) * n_trials + i, selection[(kk - 1) * n_trials + i] + 1]
    
  }
  
}




# fit model in stan

outcomes <- matrix(chosen_outcomes, ncol = N)
choices <- matrix(selection, ncol = N)

data_list <- list(N, n_games, n_trials, outcomes, choices)

fit <- stan(file = 'stan/simple_rl.stan', data = data_list, 
            iter = 5000, chains = 4)

print(fit)
plot(fit)
traceplot(fit)


rl_sim <- function(Q, N, n_trials, outcomes, choices){
  
  alpha <- Q[1]
  phi <- Q[2]
  
  current_imp <- c(0, 0)
  selprobs <- matrix(NA, nrow = n_trials, ncol = 2)
  model_lik <- vector("double", n_trials)
  
  for (i in 1:n_trials){
    
    if (i != 1){
      ind <- choices[i - 1] + 1
      current_imp[ind] <- (1 - alpha) * current_imp[ind] + alpha * outcomes[i - 1, 1]
    }
    
    selprobs[i,] <- exp(phi * current_imp) / sum(exp(phi * current_imp))
    
    selprobs[i, selprobs[i,] > .999] <- .999
    selprobs[i, selprobs[i,] < .001] <- .001
    
    model_lik[i] <- selprobs[i, choices[i, 1] + 1]
    
  }
  
  log_lik <- -2 * sum(log(model_lik))
  
}


optim(c(.3, .5), rl_sim,
      method = "L-BFGS-B",
      lower = c(0, 0),
      upper = c(1, Inf),
      N = N, n_trials = n_trials, outcomes = outcomes, choices = choices)
