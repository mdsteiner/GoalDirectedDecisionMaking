# reinforcement learning models fit

# Logistic Choice Rule
chru <- function(a, b, phi, tr){
  1/(1 + exp(-(log10(tr)**phi)*(a - b)))
} 

# Rescorla-Wagner prediction error updating function
rw.fun <- function(exp.prior = c(5, 3),      # A vector of prior expectations
                   new.inf = c(NA, 2),       # A vector of new information (NAs except for selected option)
                   alpha = .3) {   # Updating rate
  
  # Save new expectations as prior
  exp.new <- exp.prior
  
  # Determine which option was selected
  selection <- which(is.finite(new.inf))
  
  # Update expectation of selected option
  exp.new[selection] <- exp.prior[selection] + alpha * (new.inf[selection] - exp.prior[selection])
  
  return(exp.new)
  
}


# reinforcement learning fitting function
RL_Fit <- function(Q, chd, v_As, v_Bs, exp_prior_start, n_games, n_trials,
                   pred = FALSE){
  
  # fitted parameters
  alpha_fit <- Q[1]
  phi_fit <- Q[2]
  
  exp.prior.mtx <- matrix(NA, nrow = length(chd), ncol = 2)
  
  
  for (kk in seq_len(n_games)){
    
    exp.prior.mtx.temp <- matrix(NA, nrow = n_trials, ncol = 2)
    exp.prior.mtx.temp[1,] <- exp_prior_start
    
    for (ll in 2:n_trials){
      
      exp.prior.mtx.temp[ll,] <- rw.fun(exp.prior.mtx.temp[ll - 1,],
                                        c(v_As[ll - 1], v_Bs[ll - 1]),
                                        alpha_fit)
      
    }
    
    
    exp.prior.mtx[(kk * n_trials - 24) : (kk * n_trials),] <- exp.prior.mtx.temp
    
    
  }
  
  eee <- rep(NA, length(chd))
  
  trial_seq <- rep(1:25, 10)
  
  eee <- chru(exp.prior.mtx[, 1], exp.prior.mtx[, 2], phi_fit, trial_seq)
  
  eee[eee > .99999] <- .99999
  eee[eee < .00001] <- .00001
  
  if(pred==TRUE) return((round(eee) == chd))
  
  Gsq <- 2 * sum(chd[chd!=0] * (log(chd[chd!=0]) - log(eee[chd!=0])))
  
  return(Gsq)
  
  
}