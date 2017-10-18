rm(list = ls())
gc()


# --------------------------
# Section 0: Load Libraries
# --------------------------

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(afex)) install.packages("afex"); library(afex)
if (!require(coin)) install.packages("coin"); library(coin)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)

if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.long <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
### Models -------------------------

# expected utility
eu <- function(outc, alpha){
  sign(outc) * abs(outc) ** alpha
}

## pt value function
pt <- function(x, alpha, lambda){
  if (length(x) == 1){
    if (!is.na(x)){
      if (x < 0){
        return(lambda * sign(x) * abs(x) ** alpha)
      } else {
        return(x**alpha)
      }
    } else {
      return(NA)
    }
  } else if (length(x) > 1) {
    sapply(seq_along(x), function(i, xi, la, al){
      if (!is.na(xi[i])){
        if (xi[i] < 0){
          return(la * sign(xi[i]) * abs(xi[i]) ** al)
        } else {
          return(xi[i] ** al)
        }
      } else {
        return(NA)
      }
    }, xi = x, la = lambda, al = alpha)
    
  }
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

# Rescorla-Wagner prediction error updating function
rw.fun <- function(exp.prior = c(5, 3, 7),      # A vector of prior expectations
                   new.inf = 2,        # A vector of new information (NAs except for selected option)
                   selection = 1,
                   alpha.pt = .66,
                   lambda.pt = 1.15,
                   alpha.eu = .66,
                   model = "ev",
                   alpha = .3) {   # Updating rate
  
  # Save new expectations as prior
  exp.new <- exp.prior
  
  if (model == "ev"){
    # Update expectation of selected option
    exp.new[selection] <- exp.prior[selection] + alpha * (new.inf - exp.prior[selection])
  } else if (model == "eu"){
    # Update expectation of selected option
    exp.new[selection] <- exp.prior[selection] + alpha * (eu(new.inf, alpha.eu) - exp.prior[selection])
  } else {
    # Update expectation of selected option
    exp.new[selection] <- exp.prior[selection] + alpha * (pt(new.inf, alpha.pt, lambda.pt) - exp.prior[selection])
  }
  
  return(exp.new)
  
}

### Model Predictions ---------------

alpha.eu <- .66
alpha.pt <- .66 # from Kellen, Pachur, Hertwig (2016)
lambda.pt <- 1.15 # from Kellen, Pachur, Hertwig (2016)

o1.vec.sub.eu <- NULL
o2.vec.sub.eu <- NULL
o1.vec.sub.pt <- NULL
o2.vec.sub.pt <- NULL

sub.mean1eu <- NULL
sub.mean2eu <- NULL
sub.mean1pt <- NULL
sub.mean2pt <- NULL

# loop through trials
for (xx in 1:nrow(df.long)){
  
  # for both options compute the probability of reaching the 
  # goal if this option was chosen for the rest of the game
  if (df.long$trial[xx] == 1){
    points.needed.i <- ifelse(df.long$game[xx] == 1, 25, 100)
  } else {
    points.needed.i <- ifelse(df.long$game[xx] == 1, 25, 100) - df.long$points.cum[xx-1]
  }
  trials.left.i <- 26 - df.long$trial[xx]
  
  
  # compute the subjective probabilities, i.e. use the data they have actualy seen to get mean and sd
  
  # for each option get a vector of values the participant has seen so far in the game
  m1.vec <- df.long$outcome[df.long$id == df.long$id[xx] &
                              df.long$game == df.long$game[xx] &
                              df.long$trial < df.long$trial[xx] &
                              df.long$selection == 1]
  
  m2.vec <- df.long$outcome[df.long$id == df.long$id[xx] &
                              df.long$game == df.long$game[xx] &
                              df.long$trial < df.long$trial[xx] &
                              df.long$selection == 2]
  
  # compute mean and sds for this vector
  
  m1eu <- ifelse(length(m1.vec) > 0, mean(eu(m1.vec, alpha.eu), na.rm = TRUE), 0)
  m2eu <- ifelse(length(m2.vec) > 0, mean(eu(m2.vec, alpha.eu), na.rm = TRUE), 0)
  
  m1pt <- ifelse(length(m1.vec) > 0, mean(pt(m1.vec, alpha.pt, lambda.pt), na.rm = TRUE), 0)
  m2pt <- ifelse(length(m2.vec) > 0, mean(pt(m2.vec, alpha.pt, lambda.pt), na.rm = TRUE), 0)
  
  sd1 <- ifelse(length(m1.vec) > 1, sd(m1.vec, na.rm = TRUE), 1)
  sd2 <- ifelse(length(m2.vec) > 1, sd(m2.vec, na.rm = TRUE), 1)
  
  # append to a vector to use later
  
  sub.mean1eu <- c(sub.mean1eu, m1eu)
  sub.mean2eu <- c(sub.mean2eu, m2eu)
  
  sub.mean1pt <- c(sub.mean1pt, m1pt)
  sub.mean2pt <- c(sub.mean2pt, m2pt)
  
  # compute probabilities with these subjective distributions
  mueu.i <- c(m1eu, m2eu)
  mupt.i <- c(m1pt, m2pt)
  sigma.i <- c(sd1, sd2)
  
  p.temp.subjeu <- p.getthere.fun(points.needed = points.needed.i,
                                trials.left = trials.left.i,
                                mu = mueu.i,
                                sigma = sigma.i)
  
  p.temp.subjpt <- p.getthere.fun(points.needed = points.needed.i,
                                  trials.left = trials.left.i,
                                  mu = mupt.i,
                                  sigma = sigma.i)
  
  o1.vec.sub.eu <- c(o1.vec.sub.eu, p.temp.subjeu[1])
  o2.vec.sub.eu <- c(o2.vec.sub.eu, p.temp.subjeu[2])
  
  o1.vec.sub.pt <- c(o1.vec.sub.pt, p.temp.subjpt[1])
  o2.vec.sub.pt <- c(o2.vec.sub.pt, p.temp.subjpt[2])
  
}

# the probabilities were calculated with complete information of points in a trial
# i.e. the obtained probability is the one for the next trial.

df.long$p.getthere.1.subj.eu <- o1.vec.sub.eu
df.long$p.getthere.2.subj.eu <- o2.vec.sub.eu

df.long$p.getthere.1.subj.pt <- o1.vec.sub.pt
df.long$p.getthere.2.subj.pt <- o2.vec.sub.pt

# append subjective mean vectors
df.long$subj.mean.1.eu <- sub.mean1eu
df.long$subj.mean.2.eu <- sub.mean2eu

df.long$subj.mean.1.pt <- sub.mean1pt
df.long$subj.mean.2.pt <- sub.mean2pt

# prepare objects
choose.highvar.subj.eu <- NULL
pred.EV.eu <- NULL
pred.RSF.eu <- NULL

choose.highvar.subj.pt <- NULL
pred.EV.pt <- NULL
pred.RSF.pt <- NULL

# for objective and subjective p.getthere values, create vectors containing 1, when the p.getthere value for the 
# high variance option is higher than for the low variance option, and 0 otherwise
for (jj in 1:nrow(df.long)){
  
  # check if it was rational to choose the high variance obtion
  
  choose.highvar.subj.eu <- c(choose.highvar.subj.eu,
                           ifelse(df.long$p.getthere.2.subj.eu[jj] > df.long$p.getthere.1.subj.eu[jj], 1, 0))
  
  pred.EV.eu <- c(pred.EV.eu, ifelse(df.long$subj.mean.1.eu[jj] > df.long$subj.mean.2.eu[jj], 1,
                               ifelse(df.long$subj.mean.1.eu[jj] == df.long$subj.mean.2.eu[jj], sample(1:2, 1), 2)))
  
  pred.RSF.eu <- c(pred.RSF.eu, ifelse(df.long$p.getthere.1.subj.eu[jj] > df.long$p.getthere.2.subj.eu[jj], 1,
                                 ifelse(df.long$p.getthere.1.subj.eu[jj] == df.long$p.getthere.2.subj.eu[jj], sample(1:2, 1), 2)))
  
  
  choose.highvar.subj.pt <- c(choose.highvar.subj.pt,
                              ifelse(df.long$p.getthere.2.subj.pt[jj] > df.long$p.getthere.1.subj.pt[jj], 1, 0))
  
  pred.EV.pt <- c(pred.EV.pt, ifelse(df.long$subj.mean.1.pt[jj] > df.long$subj.mean.2.pt[jj], 1,
                                     ifelse(df.long$subj.mean.1.pt[jj] == df.long$subj.mean.2.pt[jj], sample(1:2, 1), 2)))
  
  pred.RSF.pt <- c(pred.RSF.pt, ifelse(df.long$p.getthere.1.subj.pt[jj] > df.long$p.getthere.2.subj.pt[jj], 1,
                                       ifelse(df.long$p.getthere.1.subj.pt[jj] == df.long$p.getthere.2.subj.pt[jj], sample(1:2, 1), 2)))
  
}

# append to dataframe
df.long$choose.highvar.subj.eu <- choose.highvar.subj.eu 
df.long$pred.EV.eu  <- pred.EV.eu 
df.long$pred.RSF.eu  <- pred.RSF.eu 
df.long$choose.highvar.subj.pt <- choose.highvar.subj.pt 
df.long$pred.EV.pt  <- pred.EV.pt 
df.long$pred.RSF.pt  <- pred.RSF.pt 
df.long$pred.EV.acc.eu <- ifelse(df.long$selection == df.long$pred.EV.eu, 1, 0)
df.long$pred.RSF.acc.eu <- ifelse(df.long$selection == df.long$pred.RSF.eu, 1, 0)
df.long$pred.EV.acc.pt <- ifelse(df.long$selection == df.long$pred.EV.pt, 1, 0)
df.long$pred.RSF.acc.pt <- ifelse(df.long$selection == df.long$pred.RSF.pt, 1, 0)

# save trial level dataframe
saveRDS(df.long, "data/Study1Data/useData/S1_dataTrialLevelEU.rds")




### Model Predictions Reinforcement learning---------------

df.long <- readRDS("data/Study1Data/useData/S1_dataTrialLevelEU.rds")

alpha.eu <- .66
alpha.pt <- .66 # from Kellen, Pachur, Hertwig (2016)
lambda.pt <- 1.15 # from Kellen, Pachur, Hertwig (2016)

alpha.rl <- .3 # same as in simulations

o1.vec.sub.ev <- NULL
o2.vec.sub.ev <- NULL
o1.vec.sub.eu <- NULL
o2.vec.sub.eu <- NULL
o1.vec.sub.pt <- NULL
o2.vec.sub.pt <- NULL


impr1.ev <- NULL
impr2.ev <- NULL
impr1.eu <- NULL
impr2.eu <- NULL
impr1.pt <- NULL
impr2.pt <- NULL

# loop through trials
for (xx in 1:nrow(df.long)){
  
  # for both options compute the probability of reaching the 
  # goal if this option was chosen for the rest of the game
  if (df.long$trial[xx] == 1){
    points.needed.i <- ifelse(df.long$game[xx] == 1, 25, 100)
  } else {
    points.needed.i <- ifelse(df.long$game[xx] == 1, 25, 100) - df.long$points.cum[xx-1]
  }
  
  trials.left.i <- 26 - df.long$trial[xx]
  
  
  # compute the subjective probabilities, i.e. use the data they have actualy seen to get mean and sd
  
  # for each option get a vector of values the participant has seen so far in the game
  m1.vec <- df.long$outcome[df.long$id == df.long$id[xx] &
                              df.long$game == df.long$game[xx] &
                              df.long$trial < df.long$trial[xx] &
                              df.long$selection == 1]
  
  m2.vec <- df.long$outcome[df.long$id == df.long$id[xx] &
                              df.long$game == df.long$game[xx] &
                              df.long$trial < df.long$trial[xx] &
                              df.long$selection == 2]
  
  # compute mean and sds for this vector
  
  if (df.long$trial[xx] == 1){
    impr.ev <- c(0, 0)
    
    impr.eu <- c(0, 0)
    
    impr.pt <- c(0, 0)
  } else {
    
    impr.ev <- rw.fun(exp.prior = c(impr1.ev[length(impr1.ev)], impr2.ev[length(impr2.ev)]),
                      new.inf = ifelse(df.long$selection[xx-1] == 1, m1.vec[length(m1.vec)],
                                      m2.vec[length(m2.vec)]),
                      selection = as.numeric(as.character(df.long$selection[xx-1])),
                      model = "ev", alpha = alpha.rl)
    
    impr.eu <- rw.fun(exp.prior = c(impr1.eu[length(impr1.eu)], impr2.eu[length(impr2.eu)]),
                      new.inf = ifelse(df.long$selection[xx-1] == 1, m1.vec[length(m1.vec)],
                                      m2.vec[length(m2.vec)]),
                      selection = as.numeric(as.character(df.long$selection[xx-1])),
                      alpha.eu = alpha.eu,model = "eu", alpha = alpha.rl)
    
    impr.pt <- rw.fun(exp.prior = c(impr1.pt[length(impr1.pt)], impr2.pt[length(impr2.pt)]),
                      new.inf = ifelse(df.long$selection[xx-1] == 1, m1.vec[length(m1.vec)],
                                      m2.vec[length(m2.vec)]),
                      selection = as.numeric(as.character(df.long$selection[xx-1])),
                      alpha.pt = alpha.pt, lambda.pt = lambda.pt,
                      model = "pt", alpha = alpha.rl)
    
  }
  
  
  sd1 <- ifelse(length(m1.vec) > 1, sd(m1.vec, na.rm = TRUE), 1)
  sd2 <- ifelse(length(m2.vec) > 1, sd(m2.vec, na.rm = TRUE), 1)
  
  # append to a vector to use later
  
  impr1.ev <- c(impr1.ev, impr.ev[1])
  impr2.ev <- c(impr2.ev, impr.ev[2])
  
  impr1.eu <- c(impr1.eu, impr.eu[1])
  impr2.eu <- c(impr2.eu, impr.eu[2])
  
  impr1.pt <- c(impr1.pt, impr.pt[1])
  impr2.pt <- c(impr2.pt, impr.pt[2])
  
  # compute probabilities with these subjective distributions
  sigma.i <- c(sd1, sd2)

  
  p.temp.subj.ev <- p.getthere.fun(points.needed = points.needed.i,
                                   trials.left = trials.left.i,
                                   mu = impr.ev,
                                   sigma = sigma.i)
  
  p.temp.subj.eu <- p.getthere.fun(points.needed = points.needed.i,
                                  trials.left = trials.left.i,
                                  mu = impr.eu,
                                  sigma = sigma.i)
  
  p.temp.subj.pt <- p.getthere.fun(points.needed = points.needed.i,
                                  trials.left = trials.left.i,
                                  mu = impr.pt,
                                  sigma = sigma.i)

  o1.vec.sub.ev <- c(o1.vec.sub.ev, p.temp.subj.ev[1])
  o2.vec.sub.ev <- c(o2.vec.sub.ev, p.temp.subj.ev[2])
  
  o1.vec.sub.eu <- c(o1.vec.sub.eu, p.temp.subj.eu[1])
  o2.vec.sub.eu <- c(o2.vec.sub.eu, p.temp.subj.eu[2])
  
  o1.vec.sub.pt <- c(o1.vec.sub.pt, p.temp.subj.pt[1])
  o2.vec.sub.pt <- c(o2.vec.sub.pt, p.temp.subj.pt[2])
  
}

# the probabilities were calculated with complete information of points in a trial
# i.e. the obtained probability is the one for the next trial.

df.long$p.getthere.1.subj.ev.rl <- o1.vec.sub.ev
df.long$p.getthere.2.subj.ev.rl <- o2.vec.sub.ev


df.long$p.getthere.1.subj.eu.rl <- o1.vec.sub.eu
df.long$p.getthere.2.subj.eu.rl <- o2.vec.sub.eu

df.long$p.getthere.1.subj.pt.rl <- o1.vec.sub.pt
df.long$p.getthere.2.subj.pt.rl <- o2.vec.sub.pt

# append subjective mean vectors
df.long$subj.mean.1.ev.rl <- impr1.ev
df.long$subj.mean.2.ev.rl <- impr2.ev

df.long$subj.mean.1.eu.rl <- impr1.eu
df.long$subj.mean.2.eu.rl <- impr2.eu

df.long$subj.mean.1.pt.rl <- impr1.pt
df.long$subj.mean.2.pt.rl <- impr2.pt

# prepare objects
choose.highvar.subj.ev <- NULL
pred.EV.ev <- NULL
pred.RSF.ev <- NULL

choose.highvar.subj.eu <- NULL
pred.EV.eu <- NULL
pred.RSF.eu <- NULL

choose.highvar.subj.pt <- NULL
pred.EV.pt <- NULL
pred.RSF.pt <- NULL

# for objective and subjective p.getthere values, create vectors containing 1, when the p.getthere value for the 
# high variance option is higher than for the low variance option, and 0 otherwise
for (jj in 1:nrow(df.long)){
  
  # check if it was rational to choose the high variance obtion
  
  choose.highvar.subj.ev <- c(choose.highvar.subj.ev,
                              ifelse(df.long$p.getthere.2.subj.ev.rl[jj] > df.long$p.getthere.1.subj.ev.rl[jj], 1, 0))
  
  pred.EV.ev <- c(pred.EV.ev, ifelse(df.long$subj.mean.1.ev.rl[jj] > df.long$subj.mean.2.ev.rl[jj], 1,
                                     ifelse(df.long$subj.mean.1.ev.rl[jj] == df.long$subj.mean.2.ev.rl[jj], sample(1:2, 1), 2)))
  
  pred.RSF.ev <- c(pred.RSF.ev, ifelse(df.long$p.getthere.1.subj.ev.rl[jj] > df.long$p.getthere.2.subj.ev.rl[jj], 1,
                                       ifelse(df.long$p.getthere.1.subj.ev.rl[jj] == df.long$p.getthere.2.subj.ev.rl[jj], sample(1:2, 1), 2)))
  
  choose.highvar.subj.eu <- c(choose.highvar.subj.eu,
                              ifelse(df.long$p.getthere.2.subj.eu.rl[jj] > df.long$p.getthere.1.subj.eu.rl[jj], 1, 0))
  
  pred.EV.eu <- c(pred.EV.eu, ifelse(df.long$subj.mean.1.eu.rl[jj] > df.long$subj.mean.2.eu.rl[jj], 1,
                                     ifelse(df.long$subj.mean.1.eu.rl[jj] == df.long$subj.mean.2.eu.rl[jj], sample(1:2, 1), 2)))
  
  pred.RSF.eu <- c(pred.RSF.eu, ifelse(df.long$p.getthere.1.subj.eu.rl[jj] > df.long$p.getthere.2.subj.eu.rl[jj], 1,
                                       ifelse(df.long$p.getthere.1.subj.eu.rl[jj] == df.long$p.getthere.2.subj.eu.rl[jj], sample(1:2, 1), 2)))
  
  
  choose.highvar.subj.pt <- c(choose.highvar.subj.pt,
                              ifelse(df.long$p.getthere.2.subj.pt.rl[jj] > df.long$p.getthere.1.subj.pt.rl[jj], 1, 0))
  
  pred.EV.pt <- c(pred.EV.pt, ifelse(df.long$subj.mean.1.pt.rl[jj] > df.long$subj.mean.2.pt.rl[jj], 1,
                                     ifelse(df.long$subj.mean.1.pt.rl[jj] == df.long$subj.mean.2.pt.rl[jj], sample(1:2, 1), 2)))
  
  pred.RSF.pt <- c(pred.RSF.pt, ifelse(df.long$p.getthere.1.subj.pt.rl[jj] > df.long$p.getthere.2.subj.pt.rl[jj], 1,
                                       ifelse(df.long$p.getthere.1.subj.pt.rl[jj] == df.long$p.getthere.2.subj.pt.rl[jj], sample(1:2, 1), 2)))
  
}

# append to dataframe
df.long$choose.highvar.subj.ev.rl <- choose.highvar.subj.ev 
df.long$pred.EV.ev.rl  <- pred.EV.ev 
df.long$pred.RSF.ev.rl  <- pred.RSF.ev 
df.long$choose.highvar.subj.eu.rl <- choose.highvar.subj.eu 
df.long$pred.EV.eu.rl  <- pred.EV.eu 
df.long$pred.RSF.eu.rl  <- pred.RSF.eu 
df.long$choose.highvar.subj.pt.rl <- choose.highvar.subj.pt 
df.long$pred.EV.pt.rl  <- pred.EV.pt 
df.long$pred.RSF.pt.rl  <- pred.RSF.pt 
df.long$pred.EV.acc.ev.rl <- ifelse(df.long$selection == df.long$pred.EV.ev.rl, 1, 0)
df.long$pred.RSF.acc.ev.rl <- ifelse(df.long$selection == df.long$pred.RSF.ev.rl, 1, 0)
df.long$pred.EV.acc.eu.rl <- ifelse(df.long$selection == df.long$pred.EV.eu.rl, 1, 0)
df.long$pred.RSF.acc.eu.rl <- ifelse(df.long$selection == df.long$pred.RSF.eu.rl, 1, 0)
df.long$pred.EV.acc.pt.rl <- ifelse(df.long$selection == df.long$pred.EV.pt.rl, 1, 0)
df.long$pred.RSF.acc.pt.rl <- ifelse(df.long$selection == df.long$pred.RSF.pt.rl, 1, 0)

# save trial level dataframe
saveRDS(df.long, "data/Study1Data/useData/S1_dataTrialLevelEU.rds")


### Participant Level Data ------------

# aggregate important variables to game level

df.participant <- df.long %>%
  filter(game > 1) %>%
  group_by(id, condition, goal.condition, variance.condition) %>%
  summarise(
    switch.rate = mean(switched, na.rm = TRUE),
    points.cum = sum(outcome),
    resp.time.median = median(resp.time, na.rm = TRUE),
    goalReachedRate = mean(goalReached),
    nGoalsReached = max(as.numeric(as.character(nGoalsReached))),
    checkFails = max(as.numeric(as.character(checkFails))),
    whichHighEV = max(as.numeric(as.character(whichHighEV))),
    overGoal.rate = mean(overGoal, na.rm = TRUE),
    high.var.chosen.rate = mean(high.var.chosen, na.rm = T),
    highEV.rate = mean(highEV, na.rm = T),
    p.getthere.1.mean = mean(p.getthere.1, na.rm = TRUE),
    p.getthere.2.mean = mean(p.getthere.2, na.rm = TRUE),
    p.getthere.1.subj.mean = mean(p.getthere.1.subj, na.rm = TRUE),
    p.getthere.2.subj.mean = mean(p.getthere.2.subj, na.rm = TRUE),
    subj.mean.1.end = subj.mean.1[length(subj.mean.1)],
    subj.mean.2.end = subj.mean.2[length(subj.mean.2)],
    choose.highvar.rate = mean(choose.highvar, na.rm = TRUE),
    choose.highvar.subj.rate = mean(choose.highvar.subj, na.rm = TRUE),
    pred.EV.acc.rate = mean(pred.EV.acc, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc, na.rm = TRUE),
    
    p.getthere.1.subj.eu.mean = mean(p.getthere.1.subj.eu, na.rm = TRUE),
    p.getthere.2.subj.eu.mean = mean(p.getthere.2.subj.eu, na.rm = TRUE),
    p.getthere.1.subj.pt.mean = mean(p.getthere.1.subj.pt, na.rm = TRUE),
    p.getthere.2.subj.pt.mean = mean(p.getthere.2.subj.pt, na.rm = TRUE),
    p.getthere.1.subj.ev.rl.mean = mean(p.getthere.1.subj.ev.rl, na.rm = TRUE),
    p.getthere.2.subj.ev.rl.mean = mean(p.getthere.2.subj.ev.rl, na.rm = TRUE),
    p.getthere.1.subj.eu.rl.mean = mean(p.getthere.1.subj.eu.rl, na.rm = TRUE),
    p.getthere.2.subj.eu.rl.mean = mean(p.getthere.2.subj.eu.rl, na.rm = TRUE),
    p.getthere.1.subj.pt.rl.mean = mean(p.getthere.1.subj.pt.rl, na.rm = TRUE),
    p.getthere.2.subj.pt.rl.mean = mean(p.getthere.2.subj.pt.rl, na.rm = TRUE),
    
    
    subj.mean.1.eu.end = subj.mean.1.eu[length(subj.mean.1.eu)],
    subj.mean.2.eu.end = subj.mean.2.eu[length(subj.mean.2.eu)],
    subj.mean.1.pt.end = subj.mean.1.pt[length(subj.mean.1.pt)],
    subj.mean.2.pt.end = subj.mean.2.pt[length(subj.mean.2.pt)],
    subj.mean.1.ev.rl.end = subj.mean.1.ev.rl[length(subj.mean.1.ev.rl)],
    subj.mean.2.ev.rl.end = subj.mean.2.ev.rl[length(subj.mean.2.ev.rl)],
    subj.mean.1.eu.rl.end = subj.mean.1.eu.rl[length(subj.mean.1.eu.rl)],
    subj.mean.2.eu.rl.end = subj.mean.2.eu.rl[length(subj.mean.2.eu.rl)],
    subj.mean.1.pt.rl.end = subj.mean.1.pt.rl[length(subj.mean.1.pt.rl)],
    subj.mean.2.pt.rl.end = subj.mean.2.pt.rl[length(subj.mean.2.pt.rl)],
    
    
    choose.highvar.subj.eu.rate = mean(choose.highvar.subj.eu, na.rm = TRUE),
    choose.highvar.subj.pt.rate = mean(choose.highvar.subj.pt, na.rm = TRUE),
    choose.highvar.subj.ev.rl.rate = mean(choose.highvar.subj.ev.rl, na.rm = TRUE),
    choose.highvar.subj.eu.rl.rate = mean(choose.highvar.subj.eu.rl, na.rm = TRUE),
    choose.highvar.subj.pt.rl.rate = mean(choose.highvar.subj.pt.rl, na.rm = TRUE),
    
    pred.EV.acc.eu.rate = mean(pred.EV.acc.eu, na.rm = TRUE),
    pred.EV.acc.pt.rate = mean(pred.EV.acc.pt, na.rm = TRUE),
    pred.EV.acc.ev.rl.rate = mean(pred.EV.acc.ev.rl, na.rm = TRUE),
    pred.EV.acc.eu.rl.rate = mean(pred.EV.acc.eu.rl, na.rm = TRUE),
    pred.EV.acc.pt.rl.rate = mean(pred.EV.acc.pt.rl, na.rm = TRUE),
    
    pred.RSF.acc.eu.rate = mean(pred.RSF.acc.eu, na.rm = TRUE),
    pred.RSF.acc.pt.rate = mean(pred.RSF.acc.pt, na.rm = TRUE),
    pred.RSF.acc.ev.rl.rate = mean(pred.RSF.acc.ev.rl, na.rm = TRUE),
    pred.RSF.acc.eu.rl.rate = mean(pred.RSF.acc.eu.rl, na.rm = TRUE),
    pred.RSF.acc.pt.rl.rate = mean(pred.RSF.acc.pt.rl, na.rm = TRUE)
  )

risky.ag <- NULL
risky.ug <- NULL

df.long <- filter(df.long, game > 1)

for (nn in unique(df.long$id)){
  
  risky.ag <- c(risky.ag,
                mean(df.long$selection[df.long$id == nn & df.long$points.cum >=  100]
                     == 2, na.rm = TRUE))
  risky.ug <- c(risky.ug,
                mean(df.long$selection[df.long$id == nn & df.long$points.cum < 100]
                     == 2, na.rm = TRUE))
}

df.participant$risky.ag <- risky.ag
df.participant$risky.ug <- risky.ug

list.surveys <- list.files("data/Study1Data/useData/", pattern = "_s_S1.txt", full.names = T)

# read in a survey dataframe as reference
survey.df <- read.table(list.surveys[1], header = T, sep = "\t", as.is = T)

# loop through survey dataframes
for (jj in 2:length(list.surveys)){
  
  # read in files and add to prepared dataframe
  temp.surv <- read.table(list.surveys[jj], header = T, sep = "\t", as.is = T)
  survey.df <- rbind(survey.df, temp.surv)
  
}

# create character id variable
survey.df$id <- as.character(survey.df$workerid)

# get rid of condition column, it already exists in df.participant
survey.df <- survey.df[, !(names(survey.df) %in% c("condition", "workerid"))]

# sanity check if ids are always the same
mean(df.participant$id == survey.df$id) == 1

# merge dataframes
df.part <- merge(df.participant, survey.df, by = "id")


saveRDS(df.part, "data/Study1Data/useData/S1_dataParticipantLevelEU.rds")




### Mixed effects models ---------

rm(list = ls())
gc()

if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(sjPlot)) install.packages("sjPlot"); library(sjPlot)
if (!require(sjstats)) install.packages("sjstats"); library(sjstats)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevelEU.rds")


df.trial$id.f <- as.factor(df.trial$id)
df.trial$variance.condition.f <- as.factor(df.trial$variance.condition)
df.trial$goal.condition.f <- as.factor(df.trial$goal.condition)
df.trial$choose.highvar.subj.f <- as.factor(df.trial$choose.highvar.subj)
df.trial$choose.highvar.subj.eu.f <- as.factor(df.trial$choose.highvar.subj.eu)
df.trial$choose.highvar.subj.pt.f <- as.factor(df.trial$choose.highvar.subj.pt)
### condition effect under goal-------------
# models from most complex to least complex



### choose highvar  non rl model Goal---------------

# EV
m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv)
r2(m.chv)

m.chv.n <- lme4::glmer(high.var.chosen ~ choose.highvar.subj + (1|id.f/game),
                       data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.n)
r2(m.chv.n)

# EU

m.chv.eu <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.eu)
r2(m.chv.eu)

m.chv.n.eu <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu + (1|id.f/game),
                       data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.n.eu)
r2(m.chv.n.eu)

# PT

m.chv.pt <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.pt)
r2(m.chv.pt)

m.chv.n.pt <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt + (1|id.f/game),
                       data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.n.pt)
r2(m.chv.n.pt)


### choose highvar RL Models Goal---------------

# EV
m.chv.ev.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.ev.rl + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.ev.rl)
r2(m.chv.ev.rl)

m.chv.n.ev.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.ev.rl + (1|id.f/game),
                       data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.n.ev.rl)
r2(m.chv.n.ev.rl)

# EU

m.chv.eu.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu.rl + (1|game) + (1|id.f),
                        data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.eu.rl)
r2(m.chv.eu.rl)

m.chv.n.eu.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu.rl + (1|id.f/game),
                          data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.n.eu.rl)
r2(m.chv.n.eu.rl)

# PT

m.chv.pt.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt.rl + (1|game) + (1|id.f),
                        data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.pt.rl)
r2(m.chv.pt.rl)

m.chv.n.pt.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt.rl + (1|id.f/game),
                          data = subset(df.trial, goal.condition == "Goal" & game > 1), family = binomial)
summary(m.chv.n.pt.rl)
r2(m.chv.n.pt.rl)



### choose highvar  non rl model No Goal---------------

# EV
m.chv <- lme4::glmer(high.var.chosen ~ choose.highvar.subj + (1|game) + (1|id.f),
                     data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv)
r2(m.chv)

m.chv.n <- lme4::glmer(high.var.chosen ~ choose.highvar.subj + (1|id.f/game),
                       data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.n)
r2(m.chv.n)

# EU

m.chv.eu <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu + (1|game) + (1|id.f),
                        data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.eu)
r2(m.chv.eu)

m.chv.n.eu <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu + (1|id.f/game),
                          data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.n.eu)
r2(m.chv.n.eu)

# PT

m.chv.pt <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt + (1|game) + (1|id.f),
                        data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.pt)
r2(m.chv.pt)

m.chv.n.pt <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt + (1|id.f/game),
                          data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.n.pt)
r2(m.chv.n.pt)


### choose highvar RL Models No Goal---------------

# EV
m.chv.ev.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.ev.rl + (1|game) + (1|id.f),
                           data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.ev.rl)
r2(m.chv.ev.rl)

m.chv.n.ev.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.ev.rl + (1|id.f/game),
                             data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.n.ev.rl)
r2(m.chv.n.ev.rl)

# EU

m.chv.eu.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu.rl + (1|game) + (1|id.f),
                           data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.eu.rl)
r2(m.chv.eu.rl)

m.chv.n.eu.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.eu.rl + (1|id.f/game),
                             data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.n.eu.rl)
r2(m.chv.n.eu.rl)

# PT

m.chv.pt.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt.rl + (1|game) + (1|id.f),
                           data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.pt.rl)
r2(m.chv.pt.rl)

m.chv.n.pt.rl <- lme4::glmer(high.var.chosen ~ choose.highvar.subj.pt.rl + (1|id.f/game),
                             data = subset(df.trial, goal.condition == "NoGoal" & game > 1), family = binomial)
summary(m.chv.n.pt.rl)
r2(m.chv.n.pt.rl)

