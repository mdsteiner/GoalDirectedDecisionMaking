
# ---------------------
# Helper functions
# ---------------------

get_samples <- function(samp,        # samp: History of observations
                         memory_N) {  # memory_N: Memory
  
  if (length(samp) > memory_N){
    new_samp <- samp[length(samp) - memory_N : length(samp)]
  } else if (length(samp) > 0){
    new_samp <- sample(samp, memory_N, replace = TRUE)
  } else {
    new_samp <- round(rnorm(memory_N, 0, 1))
  }
  
}


# ---------------------
# Choice rules
# ---------------------

# Softmax Choice Rule
#  version = 'log'
#  version = 'yb': From Yechiam and Busemeyer (2005)

softmax_Choice <- function(impressions,       # impressions: Vector (or list) of impressions of options
                           phi,               # phi: Choice sensitivity
                           trial_now,             # trial_now: Current trial
                           version = "log") { # version: Either 'yb' for Yechiam Busemeyer, or 'log' for log version     
  
  # Cleanup inputs
  if(class(impressions) == "list") {impressions <- unlist(impressions)}
  
  # Log version of choice rule
  if (version == "log") {
    
    p_select <- exp(log(trial_now ^ phi) * impressions) / sum(exp(log(trial_now ^ phi) * impressions))
    
  }
  
  # Yechiam and Busemeyer version
  if (version == "yb") {
    
    p_select <- exp((trial_now / 10) ^ phi * impressions) / sum(exp((trial_now / 10) ^ phi * impressions))

  }

  # Vector of selection probabilities
  return(p_select)
  
}


# ---------------------
# Impression rules
# ---------------------

# Sample Exploration impression rule
SampEx_Imp <- function(memory_N,      # N: the memory capacity. This number of observations is considered
                       selection_v,   # selection_v: Sequential selections made
                       outcome_v,     # outcome_v: Sequential outcomes observed
                       trial_max,     # trial_max: Maximum number of trials
                       trial_now,     # trial_now: Current trial
                       points_goal,   # points_goal: Points desired at goal. If Infinite, then impressions is based on mean
                       points_now){   # points_now: Points at current trial
  
  # Points needed
  points_need <- points_goal - points_now
  
  # get Recent Distribution
  RD_A <- get_samples(outcome_v[selection_v == 1], memory_N)
  RD_B <- get_samples(outcome_v[selection_v == 2], memory_N)
  
  # If goal is finite, impressions are based on likelihood of reaching goal
  if(is.infinite(points_goal) == FALSE) {
  
  # number of remaining trials
  t_rem <- trial_max - trial_now
  
  # create Recent Extrapolated Distribution
  ReD_A <- RD_A * t_rem
  ReD_B <- RD_B * t_rem
  
  # for each ReD value, check whether goal is reached or not (ReD binary)
  ReD_bin_A <- ReD_A > points_need
  ReD_bin_B <- ReD_B > points_need
  
  # calculate likelihood of reaching the goal from ReD
  pi_A <- mean(ReD_bin_A)
  pi_B <- mean(ReD_bin_B)
  
  # If values are the same, go with mean of ReD
  if(pi_A == pi_B) {
    
    impressions <- c(mean(RD_A), mean(RD_B))
    
  } else {
    
  # Impressions are likelihood of reaching goal from each option
  impressions <- c(pi_A, pi_B)
  
  }
  
  }
  
  # If goal is infinite, then just go with mean of RDs
  if(is.infinite(points_goal) == TRUE) {
    
    # Impressions are likelihood of reaching goal from each option
    impressions <- c(mean(RD_A), mean(RD_B))
    
  }

  # Return vector of impressions
  return(impressions)
  
}


# ---------------------
# Model Likelihood rules
# ---------------------

# Sample Extrapolation Likelihood
# Given a set of N and phi parameters, calculates the
#  likelihood of data

SampEx_Lik <- function(pars,             # Parameter vector [N, phi]
                       selection_v,      # selection_v: Selection vector
                       outcome_v,        # outcome_v: Outcome vector
                       trial_v,          # trial_v: Vector of trial numbers
                       game_v,           # game_v: Vector of game numbers
                       trial_max = NULL, # trial_max: Maximum number of trials in task
                       points_goal,      # points_goal: Points desired at goal. If Infinite, then impressions is based on mean
                       option_n = NULL,  # option_n: Number of options
                       game_n = NULL     # game_n: Number of games
                       ){ 
  
  # Fix missing values
  
  if(is.null(option_n)) {option_n <- max(selection_v)}
  if(is.null(game_n)) {game_n <- max(game_v)}
  if(is.null(trial_max)) {trial_max <- max(trial_v)}
  
  # Get memory_N and phi parameter values
  memory_N <- round(pars[1])
  phi <- pars[2]
  
  
  # Extract some information
  observations_n <- length(selection_v)
  
  
  
  # Placeholder for selection probabilities for all options
  lik_mtx <- data.frame(trial = trial_v,
                         game = game_v,
                        selection = selection_v,
                         lik = NA,
                         pred = NA)
  
  # Placeholders for likelihoods of selecting each option
  lik_mtx[paste0("o_", 1:option_n, "_lik")] <- NA
  
  # Loop over games
  for(game_i in 1:game_n) {
  
  # Look over trials
  for (trial_i in 1:trial_max) {

    points_now <- sum(outcome_v[game_v == game_i & trial_v < trial_i])

    
    # Get impressions
    impressions_i <- SampEx_Imp(memory_N = memory_N, 
                                selection_v = selection_v[game_v == game_i & trial_v < trial_i],
                                outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                                trial_max = trial_max,
                                trial_now = trial_i,
                                points_goal = points_goal,
                                points_now = points_now)
    
    # Get selection probabilities
    selprob_v <- softmax_Choice(impressions = impressions_i, 
                                phi = phi, 
                                trial_now = trial_i)
    
    
    # Assign absolute prediction to lik_mtx
    pred_abs <- (1:option_n)[selprob_v == max(selprob_v)]
    if(length(pred_abs) > 1) {pred_abs <- sample(pred_abs, size = 1)}
    lik_mtx$pred[lik_mtx$game == game_i & lik_mtx$trial == trial_i] <- pred_abs
    
    # Assign vector of selection probabilities to lik_mtx
    lik_mtx[lik_mtx$game == game_i & lik_mtx$trial == trial_i, paste0("o_", 1:option_n, "_lik")] <- round(selprob_v, 3)
    
    # Get likelihood of selection on current trial
    selection_i <- selection_v[trial_v == trial_i & game_v == game_i]
    selprob_i <- round(selprob_v[selection_i], 3)
    
    # Assign to selprob_mtx
    lik_mtx$lik[lik_mtx$trial == trial_i & lik_mtx$game == game_i] <- selprob_i
    
  }
    
  }
  
  # Get likelihood vector for observed selections
  lik_v <- lik_mtx$lik
  
  # Censor at extreme values
  lik_mtx$lik[lik_mtx$lik > .999] <- .999
  lik_mtx$lik[lik_mtx$lik < .001] <- .001
  
  # Calculate deviance
  deviance <- -2 * sum(log(lik_mtx$lik))
  
  # Calculate G2
  g2 <- deviance + 2 * length(pars) * log(observations_n)
  
  # Define final output
  
  output <- list(lik_mtx = lik_mtx,
                 deviance = deviance,
                 g2 = g2,
                 pars = pars)
  
  return(output)
  
}
           
          





