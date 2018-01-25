
# ---------------------
# Helper functions
# ---------------------

get_samples <- function(samp,        # samp: History of observations
                        memory_N) {  # memory_N: Memory
  
  
  # If there are more samples than memory, take the last memory_N outcomes
  if (length(samp) > memory_N){
    
    new_samp <- samp[(length(samp) - memory_N + 1) : length(samp)]
  }
  
  # If memory is larger than sample, then take sample
  if(length(samp) <= memory_N) {
    
    new_samp <- samp
    
  }
  
  # If sample is empty, draw a large random sample
  if(length(samp) == 0) {
    
    new_samp <- rnorm(n = 100, mean = 0, sd = 1)
    
  }

  return(new_samp)
  
}


# What is the probability I will reach the goal?
p_getthere <- function(points_need,    # points_need: How many points do I need?
                       trial_rem,      # trial_rem: Trials remaining
                       mu,             # mu: Mean of distribution(s)
                       sigma) {        # SD of distribution(s)
  
  n.options <- length(mu)
  
  output <- sapply(1:n.options, FUN = function(x) {
    
    1 - pnorm(q = points_need,                     # points desired
              mean = mu[x] * trial_rem,            # Mean
              sd = sqrt(trial_rem * sigma[x] ^ 2)) # Sd
    
  })
  
  # Set all probabilities where sigma is 0 to .5
  output[sigma == 0 | is.na(sigma)] <- .5
  
  return(output)
  
}

# ---------------------
# Choice rules
# ---------------------

# Softmax Choice Rule
#  version = 'log'
#  version = 'yb': From Yechiam and Busemeyer (2005)

Softmax_Choice <- function(impressions,       # impressions: Vector (or list) of impressions of options
                           phi,               # phi: Choice sensitivity
                           trial_now,             # trial_now: Current trial
                           version = "log") { # version: Either 'yb' for Yechiam Busemeyer, or 'log' for log version     
  
  # Cleanup inputs
  if(class(impressions) == "list") {impressions <- unlist(impressions)}
  
  # Log version of choice rule
  if (version == "log") {
    
    p_select <- exp(log(trial_now) * phi * impressions) / sum(exp(log(trial_now) * phi * impressions))
    
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
SampEx_Imp <- function(method_extrap = "heuristic",  # method_extrap: 'heuristic' or 'integration'
                       memory_N,      # N: the memory capacity. This number of observations is considered
                       selection_v,   # selection_v: Sequential selections made
                       outcome_v,     # outcome_v: Sequential outcomes observed
                       trial_max,     # trial_max: Maximum number of trials
                       trial_now,     # trial_now: Current trial
                       points_goal,   # points_goal: Points desired at goal. If Infinite, then impressions is based on mean
                       points_now){   # points_now: Points at current trial
  
  # Points needed
  points_need <- points_goal - points_now
  
  # number of remaining trials
  trial_rem <- trial_max - trial_now
  
  # get Recent Distribution
  RD_A <- get_samples(outcome_v[selection_v == 1], memory_N)
  RD_B <- get_samples(outcome_v[selection_v == 2], memory_N)
  
  if(method_extrap == "heuristic") {
  
  # If goal is finite, impressions are based on likelihood of reaching goal
  if(is.infinite(points_goal) == FALSE) {
  

  # create Recent Extrapolated Distribution
  ReD_A <- RD_A * trial_rem
  ReD_B <- RD_B * trial_rem
  
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

  }
  
  if(method_extrap == "integration") {
    
    # Impressions are based on the probability of reaching the goal given integration
    
    impressions <- c(p_getthere(points_need = points_need, 
                                trial_rem = trial_rem, 
                                mu = mean(RD_A), 
                                sigma = sd(RD_A)),
                     
                     p_getthere(points_need = points_need, 
                                trial_rem = trial_rem, 
                                mu = mean(RD_B), 
                                sigma = sd(RD_B)))
  }
  
  
  # Return vector of impressions
  return(impressions)
  
}


# mean rule
Mean_Imp <- function(selection_v,   # selection_v: Sequential selections made
                     outcome_v,     # outcome_v: Sequential outcomes observed
                     trial_max){    # trial_max: Maximum number of trials
  
  
  
  # get Recent Distribution, i.e. all sampled values so far
  RD_A <- get_samples(outcome_v[selection_v == 1], trial_max)
  RD_B <- get_samples(outcome_v[selection_v == 2], trial_max)
  
  
  # Impressions are the means of the outcomes seen
  impressions <- c(mean(RD_A), mean(RD_B))
    
  
  # Return vector of impressions
  return(impressions)
  
}


# Reinforcement learning updating rule
RL_Imp <- function(alpha_par,     # alpha_par: Updating rate [0, Inf]
                   selection_v,   # selection_v: Sequential selections made
                   outcome_v,     # outcome_v: Sequential outcomes observed
                   option_n = NULL # option_n: Number of options
                        ){   # points_now: Points at current trial


  if(is.null(option_n)) {option_n <- length(unique(selection_v))}
  
  # Get prior outcomes of each option
  prior_outcomes <- lapply(1:option_n, FUN = function(x) {outcome_v[selection_v == x]})
  
  impressions <- sapply(1:option_n, FUN = function(x) {
    
    # Get prior outcomes of option x
    prior_outcomes_x <- prior_outcomes[[x]]
    
    # If there are no outcomes, set to 0
    if(length(prior_outcomes_x) == 0) {return(0)}
    
    # If there is just one outcome, set to that outcome
    if(length(prior_outcomes_x) == 1) {return(prior_outcomes_x)}
    
    # If there is more than one outcome, use updating rule
    if(length(prior_outcomes_x) > 1) {
      
      # Start with initial outcome
      impression_x <- prior_outcomes_x[1]
      
      # Recursively update with new information
      for(i in 2:length(prior_outcomes_x)) {
        
        # Weight on new information is (1 / outcomes) & alpha_par
        weight_new <- 1 / i ^ alpha_par
        
        # Update impression
        impression_x <- (1 - weight_new) * impression_x + weight_new * prior_outcomes_x[i]
        
      }
      
    }  
  
    return(impression_x)
    
  })
  
  # Return vector of impressions
  return(impressions)
  
}


# A version of a reinforcement learning rule that tries to take goals into account
# IN PROGRESS
RLGoal_Imp <- function(alpha_par,     # alpha_par: Updating rate [0, Inf]
                       selection_v,   # selection_v: Sequential selections made
                       outcome_v,     # outcome_v: Sequential outcomes observed
                       goal = NA,     # Goal
                       curvature_par = 0.88,   # Curvature of value function
                       lambda_par = 2.25, # Loss aversion
                       option_n = NULL # option_n: Number of options
){
  
  # 
  # alpha_par = .3     # alpha_par: Updating rate [0, Inf]
  # selection_v = c(1, 2, 2, 1, 1, 2, 1, 2, 2, 2, 1, 1)  # selection_v: Sequential selections made
  # outcome_v = c(4, 2, -4, 6, 3, 5, 2, 5, -10, 2, -5, 15)     # outcome_v: Sequential outcomes observed
  # goal = 10     # Goal
  # curvature_par = 0.88   # Curvature of value function
  # lambda_par = 2.25 # Loss aversion
  # option_n = NULL
  # 
  # Get trials
  trial_max <- length(selection_v)
  
  # Get number of options
  if(is.null(option_n)) {option_n <- length(unique(selection_v))}
  
  # If there are no outcomes, set to 0
  if(trial_max == 0) {return(rep(0, option_n))}
  
  # Get cumulative earnings
  points_cum <- cumsum(outcome_v)
  
  
  # data_df stores main information
  data_df <- data.frame(trial = 1:trial_max,
                        selection = selection_v,
                        outcome = outcome_v)
  
  data_df <- data_df %>%
    mutate(
      points_cum = cumsum(outcome),
      goal_dist = points_cum - goal
    )
  
  # Create impression_df
  
  impression_df <- as.data.frame(matrix(NA, nrow = trial_max, ncol = option_n))
  names(impression_df) <- paste0("imp_", 1:option_n)
  
  data_df <- cbind(data_df, impression_df)
  
  # Loop over trials
  
  for(trial_i in 1:trial_max) {
    
    # Get prior impression of all options
    if(trial_i == 1) {imp_prior <- rep(0, option_n)}
    if(trial_i > 1) {imp_prior <- data_df[trial_i - 1, paste0("imp_", 1:option_n)]}
    
    # Calculate new impression for each option
    imp_new <- rep(NA, option_n)
    
    for(option_i in 1:option_n) {
      
      # If option is not selected, set new impression to old one
      if(data_df$selection[trial_i] != option_i) {
        
        imp_new[option_i] <- imp_prior[option_i]
        
      }
      
      # If option is selected...
      
      if(data_df$selection[trial_i] == option_i) {
        
        points_needed <- ifelse(trial_i == 1, goal, goal - data_df$points_cum[trial_i - 1])
        
        # Reference point is outcome minus points needed divided by trials remaining
        points_ref <- data_df$outcome[trial_i] - points_needed / (trial_max - trial_i + 1)
        
        # Transform points_ref via value function
        if(points_ref > 0) {
          
          points_ref <- points_ref ^ curvature_par} else {
            
            points_ref <- -lambda_par * ((-points_ref) ^ curvature_par)}
        
        weight_new <- 1 / trial_i ^ alpha_par
        
        # new information is 
        imp_new[option_i] <- (1 - weight_new) * imp_prior[option_i] + weight_new * points_ref
        
        
        }
    
    }
    
    data_df[trial_i, paste0("imp_", 1:option_n)] <- imp_new
    

  }
  
  impression_x <- data_df[trial_max, paste0("imp_", 1:option_n)]
  
    return(impression_x)
    
}
  


# ---------------------
# Model Likelihood
# ---------------------

# Model_lik
# Given a set of N and phi parameters, calculates the
#  likelihood of data

Model_Lik <- function(rule_Choice,      # rule_Choice: Choice rule [Softmax_Choice]
                      rule_Imp,         # rule_Imp: Impression rule [SampEx_Heur_Imp, SampEx_Int_Imp, RL_Imp]
                      pars_Choice,      # pars_Choice: Choice parameters
                      pars_Imp,         # pars_Imp: Impression parameters
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
  
    # Get impressions
    
  if(grepl("SampEx", rule_Imp)) {
    
    # Get points_now
    points_now <- sum(outcome_v[game_v == game_i & trial_v < trial_i])
      
    # Get memory_N and phi parameter values
    memory_N <- pars_Imp[1]
    
    if(grepl("Heur", rule_Imp)) {method_extrap <- "heuristic"}
    if(grepl("Int", rule_Imp)) {method_extrap <- "integration"}
    
    # Get impressions
    impressions_i <- SampEx_Imp(method_extrap = method_extrap,
                                memory_N = memory_N, 
                                selection_v = selection_v[game_v == game_i & trial_v < trial_i],
                                outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                                trial_max = trial_max,
                                trial_now = trial_i,
                                points_goal = points_goal,
                                points_now = points_now)
    
    }
    
    if(rule_Imp %in% c("RL")) {
      
      # Get memory_N and phi parameter values
      alpha_par <- pars_Imp[1]
      
      # Get impressions
      impressions_i <- RL_Imp(alpha_par = alpha_par,
                              selection_v = selection_v[game_v == game_i & trial_v < trial_i],
                              outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                              option_n = option_n)
      
    }
    
    if(rule_Imp %in% c("RLGoal")){
      
      # Get memory_N and phi parameter values
      alpha_par <- pars_Imp[1]
      curvature_par <- pars_Imp[2]
      lambda_par <- pars_Imp[3]
      
      # Get impressions
      impressions_i <- RLGoal_Imp(alpha_par = alpha_par,
                                  selection_v = selection_v[game_v == game_i & trial_v < trial_i], 
                                  outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                                  goal = points_goal,
                                  curvature_par = curvature_par,
                                  lambda_par = lambda_par,
                                  option_n = option_n)
      
    }
    
    if (rule_Imp %in% c("Mean")){
      
      # Get impressions
      impressions_i <- Mean_Imp(selection_v = selection_v[game_v == game_i & trial_v < trial_i],
                                outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                                trial_max = trial_max)
      
    }
    
    # Get choice probs
    
    if(rule_Choice == "Softmax") {
    
    phi_par <- pars_Choice[1]
      
    # Get selection probabilities
    selprob_v <- Softmax_Choice(impressions = impressions_i, 
                                phi = phi_par, 
                                trial_now = trial_i)
    

    }
    
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
  
  # Calculate deviance: -2 times sum of log likelihoods
  deviance <- -2 * sum(log(lik_mtx$lik))
  
  # Calculate bic
  # BIC = -2 * ln(Lik) + K * log(N)
  #  Based on Lewandowsky & Farrell, Computational Modeling in Cognition, p 184, equation 5.13
  
  if (rule_Imp %in% c("Mean")){
    pars_Imp <- NULL
  }
  
  pars_total <- length(pars_Imp) + length(pars_Choice)
  
  bic <- deviance + pars_total * log(observations_n)
  
  # Define final output
  
  output <- list(lik_mtx = lik_mtx,
                 deviance = deviance,
                 bic = bic,
                 pars_Choice = pars_Choice,
                 pars_Imp = pars_Imp)
  
  return(output)
  
}
           
          


# ---------------------
# Model Simulation
# ---------------------

# Model_Sim
# Given a set of N and phi parameters, simulates choice data

Model_Sim <- function(rule_Choice,      # rule_Choice: Choice rule [Softmax_Choice]
                      rule_Imp,         # rule_Imp: Impression rule [SampEx_Heur_Imp, SampEx_Int_Imp, RL_Imp]
                      pars_Choice,      # pars_Choice: Choice parameters
                      pars_Imp,         # pars_Imp: Impression parameters
                      selection_v,      # selection_v: Selection vector
                      outcome_v,        # outcome_v: Outcome vector
                      trial_v,          # trial_v: Vector of trial numbers
                      game_v,           # game_v: Vector of game numbers
                      trial_max = NULL, # trial_max: Maximum number of trials in task
                      points_goal,      # points_goal: Points desired at goal. If Infinite, then impressions is based on mean
                      option_n = NULL,  # option_n: Number of options
                      game_n = NULL,    # game_n: Number of games
                      outcome_mat       # Matrix containing the possible outcomes
){ 
  
  # Fix missing values
  
  if(is.null(option_n)) {option_n <- max(selection_v)}
  if(is.null(game_n)) {game_n <- max(game_v)}
  if(is.null(trial_max)) {trial_max <- max(trial_v)}
  
  # Extract some information
  observations_n <- length(selection_v)
  
  # Placeholder for selection probabilities for all options
  sim_mtx <- data.frame(trial = trial_v,
                        game = game_v,
                        pred = NA,
                        outcome = NA)
  
  
  # Loop over games
  for(game_i in 1:game_n) {
    
    # Look over trials
    for (trial_i in 1:trial_max) {
      
      # if(trial_i == 1) {
      #   selection_v[is.na(selection_v)][1] <- sample(1:option_n, 1)
      #   }
      
      # Get impressions
      
      if(grepl("SampEx", rule_Imp)) {
        
        # Get points_now
        points_now <- sum(outcome_v[game_v == game_i & trial_v < trial_i])
        
        # Get memory_N and phi parameter values
        memory_N <- pars_Imp[1]
        
        if(grepl("Heur", rule_Imp)) {method_extrap <- "heuristic"}
        if(grepl("Int", rule_Imp)) {method_extrap <- "integration"}
        
        # Get impressions
        impressions_i <- SampEx_Imp(method_extrap = method_extrap,
                                    memory_N = memory_N, 
                                    selection_v = selection_v[game_v == game_i & trial_v < trial_i],
                                    outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                                    trial_max = trial_max,
                                    trial_now = trial_i,
                                    points_goal = points_goal,
                                    points_now = points_now)
        
        
      }
      
      if(rule_Imp %in% c("RL")) {
        
        # Get memory_N and phi parameter values
        alpha_par <- pars_Imp[1]
        
        # Get impressions
        impressions_i <- RL_Imp(alpha_par = alpha_par,
                                selection_v = selection_v[game_v == game_i & trial_v < trial_i],
                                outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                                option_n = option_n)
        
        
      }
      
      
      if(rule_Imp %in% c("RLGoal")){
        
        # Get memory_N and phi parameter values
        alpha_par <- pars_Imp[1]
        curvature_par <- pars_Imp[2]
        lambda_par <- pars_Imp[3]
        
        # Get impressions
        impressions_i <- RLGoal_Imp(alpha_par = alpha_par,
                                   selection_v = selection_v[game_v == game_i & trial_v < trial_i], 
                                   outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                                   goal = points_goal,
                                   curvature_par = curvature_par,
                                   lambda_par = lambda_par,
                                   option_n = option_n)
        
      }
      
      if (rule_Imp %in% c("Mean")){
        
        # Get impressions
        impressions_i <- Mean_Imp(selection_v = selection_v[game_v == game_i & trial_v < trial_i],
                                  outcome_v = outcome_v[game_v == game_i & trial_v < trial_i],
                                  trial_max = trial_max)
        
      }
      
      # Get choice probs
      
      if(rule_Choice == "Softmax") {
        
        phi_par <- pars_Choice[1]
        
        # Get selection probabilities
        selprob_v <- Softmax_Choice(impressions = impressions_i, 
                                    phi = phi_par, 
                                    trial_now = trial_i)
        
        
      }
      
      # Assign absolute prediction to lik_mtx
      pred_abs <- sample(1:option_n, size = 1, prob = selprob_v)
      if(length(pred_abs) > 1) {pred_abs <- sample(pred_abs, size = 1)}
      sim_mtx$pred[sim_mtx$game == game_i & sim_mtx$trial == trial_i] <- pred_abs
      
      if(any(is.na(selection_v))) {selection_v[is.na(selection_v)][1] <- pred_abs}
      
      sample_outc <- outcome_mat[(game_i - 1) * trial_max + trial_i, pred_abs]
      if(any(is.na(outcome_v))) {outcome_v[is.na(outcome_v)][1] <- sample_outc}
      sim_mtx$outcome[sim_mtx$game == game_i & sim_mtx$trial == trial_i] <- sample_outc
      
    }
    
  }
  
  # Define final output
  
  return(sim_mtx)
  
}





