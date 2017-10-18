rm(list = ls())
gc()

# ------------------------
# code to generate variable with predictions of Risk
# Sensitivite Foraging Theory (RS)
# ------------------------


library(yarrr)
library(BayesFactor)


# Calculate the probability of earning points.needed points given trials.left, and 
#  sampling from a Normal distribution with a given mean and standard deviation

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



# here be dragons



# read in trial level file
df.trial <- readRDS("data/dataTrialLevel.rds")

# the first game was a practice game so only look at games > 1
df.trial <- subset(df.trial, game > 1)

# create empty vectors
o1.vec <- NULL
o2.vec <- NULL
o3.vec <- NULL

# these are the means and sds of the distributions of the two environments
mu <- c(3, 2, 2)
sd.l <- c(2, 2.5, 5)
sd.h <- c(5, 2.5, 2)

# loop through trials
for (xx in 1:nrow(df.trial)){
  
  # was the point value in trial xx under the goal of 135
  if (df.trial$points.cum[xx] < 135){
    
    # for all three options compute the probability of reaching the 
    # goal if this option was chosen for the rest of the game
    points.needed.i <- 135 - df.trial$points.cum[xx]
    trials.left.i <- 50 - df.trial$trial[xx]
    
    # high EV is low variance environment
    if (df.trial$condition[xx] %in% c(1,3)){
      p.temp <- p.getthere.fun(points.needed = points.needed.i,
                     trials.left = trials.left.i,
                     mu = mu,
                     sigma = sd.l)
    } else {
      # high EV is high variance environment
      p.temp <- p.getthere.fun(points.needed = points.needed.i,
                     trials.left = trials.left.i,
                     mu = mu,
                     sigma = sd.h)
    }
    
  } else {
    # do the same if point value is > the goal
    #p.temp <- c(NA, NA, NA)
    points.needed.i <- 135 - df.trial$points.cum[xx]
    trials.left.i <- 50 - df.trial$trial[xx]
    if (df.trial$condition[xx] %in% c(1,3)){
      p.temp <- p.getthere.fun(points.needed = points.needed.i,
                               trials.left = trials.left.i,
                               mu = mu,
                               sigma = sd.l)
    } else {
      p.temp <- p.getthere.fun(points.needed = points.needed.i,
                               trials.left = trials.left.i,
                               mu = mu,
                               sigma = sd.h)
    }
  }
  
  # append probability values to vector
  o1.vec <- c(o1.vec, p.temp[1])
  o2.vec <- c(o2.vec, p.temp[2])
  o3.vec <- c(o3.vec, p.temp[3])
  
}

# the probabilities were calculated with complete information of points in a trial
# i.e. the obtained probability is the one for the next trial. Thus we have to 
# reposition everything by one.
o1.vec <- c(NA, o1.vec)
o2.vec <- c(NA, o2.vec)
o3.vec <- c(NA, o3.vec)

o1.vec <- o1.vec[-length(o1.vec)]
o2.vec <- o2.vec[-length(o2.vec)]
o3.vec <- o3.vec[-length(o3.vec)]

df.trial$p.getthere.1 <- o1.vec
df.trial$p.getthere.2 <- o2.vec
df.trial$p.getthere.3 <- o3.vec

df.trial$p.getthere.1[df.trial$trial == 1] <- NA
df.trial$p.getthere.2[df.trial$trial == 1] <- NA
df.trial$p.getthere.3[df.trial$trial == 1] <- NA



#### 
# and now for the subjective stuff
###

o1.vec <- NULL
o2.vec <- NULL
o3.vec <- NULL

sub.mean1 <- NULL
sub.mean2 <- NULL
sub.mean3 <- NULL

for (xx in 1:nrow(df.trial)){

  
    
  if (df.trial$points.cum[xx] < 135){
    
    # for each option get a vector of values the participant has seen so far in the game
    m1.vec <- df.trial$outcome[df.trial$workerid == df.trial$workerid[xx] &
                                 df.trial$game == df.trial$game[xx] &
                                 df.trial$trial <= df.trial$trial[xx] &
                                 df.trial$selection == 1]
    m2.vec <- df.trial$outcome[df.trial$workerid == df.trial$workerid[xx] &
                                 df.trial$game == df.trial$game[xx] &
                                 df.trial$trial <= df.trial$trial[xx] &
                                 df.trial$selection == 2]
    m3.vec <- df.trial$outcome[df.trial$workerid == df.trial$workerid[xx] &
                                 df.trial$game == df.trial$game[xx] &
                                 df.trial$trial <= df.trial$trial[xx] &
                                 df.trial$selection == 3]
    
    # compute mean and sds for this vector
    m1 <- ifelse(length(m1.vec) > 0, mean(m1.vec, na.rm = T), 0)
    m2 <- ifelse(length(m2.vec) > 0, mean(m2.vec, na.rm = T), 0)
    m3 <- ifelse(length(m3.vec) > 0, mean(m3.vec, na.rm = T), 0)
    
    sd1 <- ifelse(length(m1.vec) > 0, sd(m1.vec, na.rm = T), 1)
    sd2 <- ifelse(length(m2.vec) > 0, sd(m2.vec, na.rm = T), 1)
    sd3 <- ifelse(length(m3.vec) > 0, sd(m3.vec, na.rm = T), 1)
    
    # append to a vector to use later
    sub.mean1 <- c(sub.mean1, m1)
    sub.mean2 <- c(sub.mean2, m2)
    sub.mean3 <- c(sub.mean3, m3)
    
    # compute probabilities with these subjective distributions
    mu.i <- c(m1, m2, m3)
    sigma.i <- c(sd1, sd2, sd3)
    
    points.needed.i <- 135 - df.trial$points.cum[xx]
    trials.left.i <- 50 - df.trial$trial[xx]

    p.temp <- p.getthere.fun(points.needed = points.needed.i,
                               trials.left = trials.left.i,
                               mu = mu.i,
                               sigma = sigma.i)
  } else {
    #p.temp <- c(NA, NA, NA)
    # do the same for over the goal
    m1.vec <- df.trial$outcome[df.trial$workerid == df.trial$workerid[xx] &
                                 df.trial$game == df.trial$game[xx] &
                                 df.trial$trial <= df.trial$trial[xx] &
                                 df.trial$selection == 1]
    m2.vec <- df.trial$outcome[df.trial$workerid == df.trial$workerid[xx] &
                                 df.trial$game == df.trial$game[xx] &
                                 df.trial$trial <= df.trial$trial[xx] &
                                 df.trial$selection == 2]
    m3.vec <- df.trial$outcome[df.trial$workerid == df.trial$workerid[xx] &
                                 df.trial$game == df.trial$game[xx] &
                                 df.trial$trial <= df.trial$trial[xx] &
                                 df.trial$selection == 3]
    
    m1 <- ifelse(length(m1.vec) > 0, mean(m1.vec, na.rm = T), 0)
    m2 <- ifelse(length(m2.vec) > 0, mean(m2.vec, na.rm = T), 0)
    m3 <- ifelse(length(m3.vec) > 0, mean(m3.vec, na.rm = T), 0)
    
    sd1 <- ifelse(length(m1.vec) > 0, sd(m1.vec, na.rm = T), 1)
    sd2 <- ifelse(length(m2.vec) > 0, sd(m2.vec, na.rm = T), 1)
    sd3 <- ifelse(length(m3.vec) > 0, sd(m3.vec, na.rm = T), 1)
    
    sub.mean1 <- c(sub.mean1, m1)
    sub.mean2 <- c(sub.mean2, m2)
    sub.mean3 <- c(sub.mean3, m3)
    
    mu.i <- c(m1, m2, m3)
    sigma.i <- c(sd1, sd2, sd3)
    
    points.needed.i <- 135 - df.trial$points.cum[xx]
    trials.left.i <- 50 - df.trial$trial[xx]
    
    p.temp <- p.getthere.fun(points.needed = points.needed.i,
                             trials.left = trials.left.i,
                             mu = mu.i,
                             sigma = sigma.i)
    
  }
  
  o1.vec <- c(o1.vec, p.temp[1])
  o2.vec <- c(o2.vec, p.temp[2])
  o3.vec <- c(o3.vec, p.temp[3])
  
}

# reposition by one
o1.vec <- c(NA, o1.vec)
o2.vec <- c(NA, o2.vec)
o3.vec <- c(NA, o3.vec)

o1.vec <- o1.vec[-length(o1.vec)]
o2.vec <- o2.vec[-length(o2.vec)]
o3.vec <- o3.vec[-length(o3.vec)]


df.trial$p.getthere.1.subj <- o1.vec
df.trial$p.getthere.2.subj <- o2.vec
df.trial$p.getthere.3.subj <- o3.vec

df.trial$p.getthere.1.subj[df.trial$trial == 1] <- NA
df.trial$p.getthere.2.subj[df.trial$trial == 1] <- NA
df.trial$p.getthere.3.subj[df.trial$trial == 1] <- NA

# append subjective mean vectors
df.trial$subj.mean.1 <- sub.mean1
df.trial$subj.mean.2 <- sub.mean2
df.trial$subj.mean.3 <- sub.mean3

# prepare objects
choose.highvar <- NULL
choose.highvar.subj <- NULL

for (jj in 1:nrow(df.trial)){
  if(df.trial$condition[jj] %in% c(1,3)){
    if (df.trial$overGoal[jj] == 0){
      
      # check if it was rational to choose the high variance obtion
      choose.highvar <- c(choose.highvar, ifelse(df.trial$p.getthere.3[jj] > df.trial$p.getthere.2[jj] &
                                                   df.trial$p.getthere.3[jj] > df.trial$p.getthere.1[jj],
                                                 1, 0))
      
      choose.highvar.subj <- c(choose.highvar.subj,
                               ifelse(df.trial$p.getthere.3.subj[jj] > df.trial$p.getthere.2.subj[jj] &
                                        df.trial$p.getthere.3.subj[jj] > df.trial$p.getthere.1.subj[jj],
                                      1, 0))
    } else{     
      # check if it was rational to choose the high variance obtion
      choose.highvar <- c(choose.highvar,
                          ifelse(df.trial$p.getthere.3[jj] > df.trial$p.getthere.2[jj] &
                                   df.trial$p.getthere.3[jj] > df.trial$p.getthere.1[jj], 1, 0))
      
      choose.highvar.subj <- c(choose.highvar.subj,
                               ifelse(df.trial$p.getthere.3.subj[jj] > df.trial$p.getthere.2.subj[jj] &
                                        df.trial$p.getthere.3.subj[jj] > df.trial$p.getthere.1.subj[jj],
                                      1, 0))  
    }
  } else{
    if (df.trial$overGoal[jj] == 0){
      # check if it was rational to choose the high variance obtion
      
      choose.highvar <- c(choose.highvar, ifelse(df.trial$p.getthere.1[jj] > df.trial$p.getthere.2[jj] &
                                                   df.trial$p.getthere.1[jj] > df.trial$p.getthere.3[jj],
                                                 1, 0))
      
      choose.highvar.subj <- c(choose.highvar.subj,
                               ifelse(df.trial$p.getthere.1.subj[jj] > df.trial$p.getthere.2.subj[jj] &
                                        df.trial$p.getthere.1.subj[jj] > df.trial$p.getthere.3.subj[jj],
                                      1, 0))
    } else{
      # check if it was rational to choose the high variance obtion
      choose.highvar <- c(choose.highvar, ifelse(df.trial$p.getthere.1[jj] > df.trial$p.getthere.2[jj] &
                                                   df.trial$p.getthere.1[jj] > df.trial$p.getthere.3[jj],
                                                 1, 0))
      
      choose.highvar.subj <- c(choose.highvar.subj,
                               ifelse(df.trial$p.getthere.1.subj[jj] > df.trial$p.getthere.2.subj[jj] &
                                        df.trial$p.getthere.1.subj[jj] > df.trial$p.getthere.3.subj[jj],
                                      1, 0))
    }
  }
}

df.trial$choose.highvar <- choose.highvar
df.trial$choose.highvar.subj <- choose.highvar.subj

saveRDS(df.trial, "data/dataTrialLevelPGetthere.rds")


# THE REST IS OLD CODE





## check condition 3
#n.df1 <- aggregate(high.var.chosen ~ choose.highvar + participant + game, FUN = mean, data = subset(df.trial, condition #== 3 & trial >= 40))
#names(n.df1)
#pirateplot(high.var.chosen ~ choose.highvar, data = n.df1, ylab = "p high var option chosen", main = "Prop high variance #chosen when rational vs not")
#
#
#generalTestBF(high.var.chosen ~ choose.highvar, data = n.df1)
#plot(generalTestBF(high.var.chosen ~ choose.highvar, data = n.df1))
#
#
## aaaand subjective
#n.df1.s <- aggregate(high.var.chosen ~ choose.highvar + participant + game, FUN = mean, data = subset(df.trial, condition #== 3 & trial >= 40))
#names(n.df1.s)
#pirateplot(high.var.chosen ~ choose.highvar, data = n.df1.s, ylab = "p high var option chosen", main = "Prop high #variance chosen when rational vs not")
#
#
#generalTestBF(high.var.chosen ~ choose.highvar, data = n.df1.s)
#plot(generalTestBF(high.var.chosen ~ choose.highvar, data = n.df1.s))
#
#
#### check condiiton 1
#n.df2 <- aggregate(high.var.chosen ~ choose.highvar + participant + game, FUN = mean, data = subset(df.trial, condition #== 1 & trial >= 40))
#names(n.df2)
#pirateplot(high.var.chosen ~ choose.highvar, data = n.df2, ylab = "p high var option chosen", main = "Prop high variance #chosen when rational vs not")
#
#
#generalTestBF(high.var.chosen ~ choose.highvar, data = n.df2)
#plot(generalTestBF(high.var.chosen ~ choose.highvar, data = n.df2))
#
## aaaand subjective
#n.df2.s <- aggregate(high.var.chosen ~ choose.highvar.subj + participant + game, FUN = mean, data = subset(df.trial, #condition == 1 & trial >= 40))
#names(n.df2.s)
#pirateplot(high.var.chosen ~ choose.highvar.subj, data = n.df2.s, ylab = "p high var option chosen", main = "Prop high #variance chosen when rational vs not")
#
#
#generalTestBF(high.var.chosen ~ choose.highvar, data = n.df2.s)
#plot(generalTestBF(high.var.chosen ~ choose.highvar, data = n.df2.s))
#
#### check condiiton 4
#n.df3 <- aggregate(high.var.chosen ~ choose.highvar + participant + game, FUN = mean, data = subset(df.trial, condition #== 4 & trial >= 40))
#names(n.df3)
#pirateplot(high.var.chosen ~ choose.highvar, data = n.df3, ylab = "p high var option chosen", main = "Prop high variance #chosen when rational vs not")
#
#
#generalTestBF(high.var.chosen ~ choose.highvar, data = n.df3)
#plot(generalTestBF(high.var.chosen ~ choose.highvar, data = n.df3))
#
#### check condiiton 2
#n.df4 <- aggregate(high.var.chosen ~ choose.highvar + participant + game, FUN = mean, data = subset(df.trial, condition #== 2 & trial >= 40))
#names(n.df4)
#pirateplot(high.var.chosen ~ choose.highvar, data = n.df4, ylab = "p high var option chosen", main = "Prop high variance #chosen when rational vs not")
#
#
#generalTestBF(high.var.chosen ~ choose.highvar, data = n.df4)
#plot(generalTestBF(high.var.chosen ~ choose.highvar, data = n.df4))
#
#
#
#
#
## create subset
#df.trial.n <- subset(df.trial, trial > 30 & condition == 3 & overGoal == 0 & game > 1)
#
## compute the point value from which on it is rational to choose the low EV but high variance
## option
#vals <- seq(0, 6, .001)
#hEV <- pnorm(vals , mean = 3, sd = 2, lower.tail = FALSE)
#hVAR <- pnorm(vals , mean = 2, sd = 5, lower.tail = FALSE)
#
#vals[which(hVAR > hEV)[1]]
#vals[which((hVAR*hVAR) > (hEV * hEV))[1]]
#vals[which((hVAR ^ 3) > (hEV ^ 3))[1]]
#
#135/50
#50 * 3.667
#pnorm(185, 2,5, lower.tail = F) > pnorm(185, 3,2, lower.tail = F)
#
#
## compute the value needed in each trial (i.e. the smallest value that, if multiplied by the number
## of trials left lets you reache the goal)
#df.trial.n$meanValNeeded <- (135 - df.trial.n$points.cum) / (50 - df.trial.n$trial)
#df.trial.n$meanValNeeded[df.trial.n$meanValNeeded == Inf] <- NA
#hist(df.trial.n$meanValNeeded)
#
## check if value was greater or smaller than 3.667, the value computed before
#new.vec <- c(NA, ifelse(df.trial.n$meanValNeeded >= 3.667, 1, 0))
#new.vec <- new.vec[-length(new.vec)]
#df.trial.n$highVarBetter <- new.vec
#
## get the mean rates of high var options chosen separated for whether this was the rational strategy
## or not
#n.df <- aggregate(high.var.chosen ~ highVarBetter + participant + game, FUN = mean, data = df.trial.n)
#names(n.df)
#pirateplot(high.var.chosen ~ highVarBetter, data = n.df, ylab = "p high var option chosen", main = "Prop high variance #chosen when rational vs not")
#
#
#generalTestBF(high.var.chosen ~ highVarBetter, data = n.df)
#plot(generalTestBF(high.var.chosen ~ highVarBetter, data = n.df))
#
#