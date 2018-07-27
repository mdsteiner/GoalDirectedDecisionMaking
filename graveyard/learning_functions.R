#
# Learning functions
#
# UPDATING
# rw.fun: Rescorla-Wagner
#
# CHOICE
# softmax.fun(exp.current, theta): Softmax
# egreedy.fun(exp.current, epsilon): epsilon-greedy
#
# OTHER
# p.getthere.fun(points.needed, trials.left, mu, sigma): What is the probability of reaching a goal?
#
# SIMULATION
#
# rl.sim.fun(): Simulate the behavior of an agent given specified parameters
#

# Rescorla-Wagner prediction error updating function
rw.fun <- function(exp.prior = c(5, 3, 7),      # A vector of prior expectations
                   new.inf = c(NA, 2, NA),        # A vector of new information (NAs except for selected option)
                   alpha = .3) {   # Updating rate
  
  # Save new expectations as prior
  exp.new <- exp.prior
  
  # Determine which option was selected
  selection <- which(is.finite(new.inf))
  
  # Update expectation of selected option
  exp.new[selection] <- exp.prior[selection] + alpha * (new.inf[selection] - exp.prior[selection])
  
  return(exp.new)
  
}

# Softmax selection function
softmax.fun <- function(exp.current = c(5, 3, 6),   # A vector of expectations
                        theta = .5,                  # Choice sensitivity
                        trial = NA
) {
  
  if(is.na(trial)) {
  
  output <- exp(exp.current * theta) / sum(exp(exp.current * theta))
  
  }
  
  return(output)
  
}

# egreedy
egreedy.fun <- function(exp.current = c(5, 3, 6),   # A vector of expectations
                        epsilon = .5                # probability of random choice
) {
  
  n.options <- length(exp.current)
  output <- rep(0,n.options)
  best <- which(exp.current == max(exp.current))
  if(length(best) > 1) {best <- sample(best, 1)}
  
  # exploit or explore?
  
  # If random number is less than epsilon, then explore
  if(runif(1, 0, 1) < epsilon) {
    
    if(n.options == 2) {
      
      selection <- setdiff(1:n.options, best)
      
    } else {
      
      selection <- sample(setdiff(1:n.options, best), size = 1)
      
    }
    
  } else {  # exploit
    
    selection <- best
    
  }
  
  output[selection] <- 1
  
  return(output)
  
}


# What is the probability I will reach the goal?
p.getthere.fun <- function(points.needed,  # How many points do I need?
                           trials.left,     # Trials remaining
                           mu,             # Mean of distribution(s)
                           sigma) {        # SD of distribution(s)
  
  n.options <- length(mu)
  
  output <- sapply(1:n.options, FUN = function(x) {
    
    1 - pnorm(q = points.needed,                     # points desired
              mean = mu[x] * trials.left,            # Mean
              sd = sqrt(trials.left * sigma[x] ^ 2)) # Sd
    
  })
  
  return(output)
  
}


# reinforcement learning fitting function
rl_fit <- function(Q, chd, outc){
  
  alpha_fit <- Q[1]
  phi_fit <- Q[2]
  
  for (kk in seq_along(chd)){
    
  }
  
  
}

# Create main simulation function
rl.sim.fun <- function(n.trials = 100,                # Trials in game
                       option.mean = c(.5, 1, 1.5),   # Mean of each option
                       option.sd = c(.1, 2, 4),       # SD of each option
                       prior.exp.start = rep(0, 3),   # Prior expectations
                       prior.sd.start = 1,            # Prior standard deviation
                       goal = 100,                    # Goal
                       epsilon = .1,                  # epsilon parameter for egreedy.fun
                       theta = .5,                    # theta parameter for softmax.fun  
                       alpha = .2,                    # alpha updating rate for rw.fun
                       selection.strat = "egreedy",   # softmax or egreedy
                       strategy = "ev",               # Either ev or rsf or random
                       int.values = FALSE,
                       plot = FALSE, 
                       ylim = NULL) {
  
  
  # TESTING
  # n.trials = 25                # Trials in game
  # option.mean = c(2, 3)   # Mean of each option
  # option.sd = c(1, 5)       # SD of each option
  # prior.exp.start = rep(0, 2)  # Prior expectations
  # prior.sd.start = 1            # Prior standard deviation
  # goal = 50                    # Goal
  # epsilon = .1                 # epsilon parameter for egreedy.fun
  # theta = .5                    # theta parameter for softmax.fun
  # alpha = .2                    # alpha updating rate for rw.fun
  # selection.strat = "softmax"   # softmax or egreedy
  # strategy = "rsf"               # Either ev or rsf
  # plot = FALSE
  # ylim = NULL
  
  
  # Get some game parameters from inputs
  n.options <- length(option.mean)
  
  # Create outcome matrix giving the outcome on each trial for each option
  outcome.mtx <- matrix(NA, nrow = n.trials, ncol = n.options)
  
  if (int.values == FALSE){
    for(option.i in 1:n.options) {
      
      outcome.mtx[,option.i] <- rnorm(n = n.trials, 
                                      mean = option.mean[option.i], 
                                      sd = option.sd[option.i])
      
    }
  } else {
    for(option.i in 1:n.options) {
      
      outcome.mtx[,option.i] <- round(rnorm(n = n.trials, 
                                      mean = option.mean[option.i], 
                                      sd = option.sd[option.i]))
      
    }
  }
  
  # Create exp.prior and exp.new matrices
  #  These will hold the agent's expectation (either prior or new) 
  #   of each option on each trial
  
  exp.prior.mtx <- matrix(NA, nrow = n.trials, ncol = n.options)
  exp.prior.mtx[1,] <- prior.exp.start
  exp.new.mtx <- matrix(NA, nrow = n.trials, ncol = n.options)
  
  # Now create some matrices to store values
  
  selection.v <- rep(NA, n.trials)      # Actual selections
  outcome.v <- rep(NA, n.trials)        # Actual outcomes
  diff.pred.v <- rep(NA, n.trials)        # differences in prediction
  selprob.mtx <- as.data.frame(matrix(NA,             # Selection probabilities
                        nrow = n.trials, 
                        ncol = n.options))
  
  names(selprob.mtx) <- paste0("sel.", letters[1:n.options])
  
  gt.mtx <- as.data.frame(matrix(NA,             # p get there
                                nrow = n.trials, 
                                ncol = n.options))
  
  names(gt.mtx) <- paste0("gt.", letters[1:n.options])
  
  
  reward.mtx <- matrix(NA, nrow = n.trials, ncol = n.options)
  
  # RUN SIMULATION!
  
  for(trial.i in 1:n.trials) {
    
    # STEP 0: Get prior expectations for current trial
    
    exp.prior.i <- exp.prior.mtx[trial.i,]
    
    # Determine probability of reaching goal for each option
    points.earned <- sum(outcome.v[1:trial.i], na.rm = TRUE)
    points.needed <- goal - sum(outcome.v[1:trial.i], na.rm = TRUE)
    
    sd.observed <- sapply(1:n.options, FUN = function(x) {sd(reward.mtx[,x], na.rm = TRUE)})
    sd.observed[is.na(sd.observed)] <- prior.sd.start
    
    trials.left <- n.trials - trial.i
    
    p.getthere <- p.getthere.fun(points.needed = points.needed,
                                 trials.left = trials.left,
                                 mu = exp.prior.i,
                                 sigma = sd.observed)
    
    
    
    
    # STEP 1: SELECT AN OPTION
    
    # Selection probabilities
    
    # EV strategy: Select according to expectations
    if(strategy == "ev") {
    
      if(selection.strat == "softmax") {
      
    selprob.i <- softmax.fun(exp.current = exp.prior.i, 
                             theta = theta)
    
    # differences in prediction
    if(mean(is.finite(p.getthere)) == 1) {
      
      if(sum(p.getthere) == 0) {
        
        selprob.diff <- rep(1 / n.options, n.options)} else {
          
          selprob.diff <- p.getthere / sum(p.getthere)
          
        }
      
    } else {selprob.i <- rep(1 / n.options, n.options)}
    
      }
      
      
      if(selection.strat == "egreedy") {
        
        selprob.i <- egreedy.fun(exp.current = exp.prior.i, 
                                 epsilon = epsilon)
        
        # differences in prediction
        
        selprob.diff <- egreedy.fun(p.getthere, epsilon)
        
      }
    
    }
    
    # RSF strategy: Select according to probability of reaching the goal
    if(strategy == "rsf") {
       
       if(selection.strat == "softmax") {
       
         if(mean(is.finite(p.getthere)) == 1) {
         
         if(sum(p.getthere) == 0) {
           
           selprob.i <- rep(1 / n.options, n.options)} else {
         
      selprob.i <- p.getthere / sum(p.getthere)
           
           }
           
         } else {selprob.i <- rep(1 / n.options, n.options)}
      
         # differences in prediction
         selprob.diff <- softmax.fun(exp.current = exp.prior.i, 
                                  theta = theta)
       }
       
       if(selection.strat == "egreedy") {
         
         selprob.i <- egreedy.fun(p.getthere, epsilon)
         
         # differences in prediction
         selprob.diff <- egreedy.fun(exp.current = exp.prior.i, 
                                  epsilon = epsilon)
       }
      
    }
    
    # Random strategy: Select at random (this is ensured by theta = 0)
    if(strategy == "random") {
      
        
      selprob.i <- rep(1 / n.options, n.options)
      
    }
    # Select an option
    
    selection.i <- sample(1:n.options, size = 1, prob = selprob.i)
    
    # differences in prediction
    if(strategy %in% c("ev", "rsf")){
      selection.diff <- sample(1:n.options, size = 1, prob = selprob.diff)
      
      diff.pred.i <- ifelse(selection.i == selection.diff, 0, 1)
    } else {
      diff.pred.i = NA
    }
    
    # Get outcome from selected option
    
    outcome.i <- outcome.mtx[trial.i, selection.i]
    
    # STEP 3: CREATE NEW EXPECTANCIES
    
    # Create a new.inf vector with NAs except for outcome of selected option
    
    new.inf <- rep(NA, n.options)
    new.inf[selection.i] <- outcome.i
    
    # Get new expectancies
    new.exp.i <- rw.fun(exp.prior = exp.prior.i,
                        new.inf = new.inf,
                        alpha = alpha)
    
    # assign new expectatations to exp.new.mtx[trial.i,]
    #  and prior.expecation.mtx[trial.i + 1,]
    
    exp.new.mtx[trial.i,] <- new.exp.i
    
    if(trial.i < n.trials) {
      
      exp.prior.mtx[trial.i + 1,] <- new.exp.i
      
    }
    
    # Save some values
    
    selprob.mtx[trial.i,] <- selprob.i  # Selection probabilities
    selection.v[trial.i] <- selection.i # Actual selection
    outcome.v[trial.i] <- outcome.i     # Actual outcome
    reward.mtx[trial.i, selection.i] <- outcome.i
    gt.mtx[trial.i,] <- p.getthere
    diff.pred.v[trial.i] <- diff.pred.i
  }
  
  # Put main results in a single dataframe called sim.result.df
  
  sim.result.df <- data.frame("selection" = selection.v, 
                              "outcome" = outcome.v,
                              "outcome.cum" = cumsum(outcome.v),
                              "diff.pred" = diff.pred.v,
                              stringsAsFactors = FALSE)
  
  sim.result.df <- cbind(sim.result.df, selprob.mtx, gt.mtx)
  
  sim.result.df
  
  # Should simulation be plotted?
  
  if(plot == TRUE) {
    
    if(is.null(ylim)) {ylim <- c(min(option.mean) * n.trials,  # Set limits to worst and best exepected performance
                                max(option.mean) * n.trials)}
    
    plot(1, 
         xlim = c(1, n.trials), 
         ylim = ylim, 
         type = "n",
         ylab = "Cumulative Rewards",
         xlab = "Trial",
         main = paste0("alpha = ", alpha, ", theta = ", theta))
    
    rect(-1e3, -1e3, 1e3, 1e3, col = gray(.96))
    abline(h = seq(-1e3, 1e3, by = 10), 
           v = seq(-1e3, 1e3, by = 10),
           lwd = c(2, 1), col = "white")
    
    points(x = 1:n.trials,
           y = sim.result.df$outcome.cum,
           type = "b")
    
    text(1:n.trials, 
         y = sim.result.df$outcome.cum, 
         labels = letters[selection.v], pos = 3)
    
    if(is.finite(goal)) {abline(h = goal)}
    
  }
  
  
  # Now return the main simulation dataframe
 
  return(sim.result.df)
  
  
}


