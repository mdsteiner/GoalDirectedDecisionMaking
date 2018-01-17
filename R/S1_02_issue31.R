rm(list = ls())
gc()

# ------------
# DATA ANALYSIS ISSUE #31
#
# Study (working title): Need based decision making in a reinforcement learning task.
# 
# Authors:
#   - Markus D. Steiner (markus.d.steiner@gmail.com)
#   - Nathaniel D. Phillips (Nathaniel.D.Phillips.is@gmail.com)
# 
# Create a plot showing the relationship between how much the two strategies (rsf v. ev) favor an
# option, and how likely it was to be chosen by participants.
# 
# e.g.
# 
# for RSF, you can calculate the probability that optionA vs. optionB gets you to the goal. Then
# calculate the difference. As a function of this difference, how likely are people to choose the
# option favored by RSF?
# 
# For EV, you could do a d-prime on the two distributions. e.g.; mean(a) / sd(a) - mean(b) / sd(b).
# The more extreme the d-prime, the more people should choose the option with the higher mean
# ------------

# --------------------------
# Section 0: Load Libraries
# --------------------------

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)


# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")

### function definition ---------------

evidence_strength <- function(df, strategy, nround, ...){
  if (strategy == "RSF") {
    fav.opt <- round(df$p.getthere.1.subj - df$p.getthere.2.subj, nround)
  } else if (strategy == "EV") {
    fav.opt <- round((df$subj.mean.1 / df$sd.sub1) - (df$subj.mean.2 / df$sd.sub2), nround)
  } else {
    stop("No valid strategy. Strategy must be either EV or RSF")
  }
  fav.opt[fav.opt == Inf | fav.opt == -Inf] <- NA
  unique.fav.opt <- unique(fav.opt)
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  p.choose.1 <- unlist(lapply(seq_along(unique.fav.opt),
                              function(x, df, fav.opt, unique.fav.opt){
                                mean(df$selection[fav.opt == unique.fav.opt[x]] == 1,
                                     na.rm = TRUE)
                              },
                              df = df, fav.opt = fav.opt, unique.fav.opt = unique.fav.opt))
  
  # order after unique.fav.opt to be able to plot lines and not only points
  p.choose.1.o <- p.choose.1[order(unique.fav.opt)]
  unique.fav.opt.o <- unique.fav.opt[order(unique.fav.opt)]
  
  # plot the results
  plot(unique.fav.opt.o, p.choose.1.o, type = "l",
       ylab = "p of Choosing Option 1", col = gray(0.3, .7), lwd = 2, cex = 1.5, cex.lab = 1.5,
       cex.axis = 1.5, ylim = c(0, 1), ...)
  
  df.evidence <- data.frame("p.choose.1" = p.choose.1.o,
                            "fav.opt" = unique.fav.opt.o)
  
  df.evidence
}


evidence_strength2 <- function(df, strategy, nround, nbatches, ...){
  if (strategy == "RSF") {
    fav.opt <- round(df$p.getthere.1.subj - df$p.getthere.2.subj, nround)
  } else if (strategy == "EV") {
    fav.opt <- round((df$subj.mean.1 / df$sd.sub1) - (df$subj.mean.2 / df$sd.sub2), nround)
  } else {
    stop("No valid strategy. Strategy must be either EV or RSF")
  }
  fav.opt[fav.opt == Inf | fav.opt == -Inf] <- NA
  
  # create batches
  batch.size <- (max(fav.opt, na.rm = TRUE) - min(fav.opt, na.rm = TRUE)) / nbatches
  
  batch.int <- cumsum(c(min(fav.opt, na.rm = TRUE), rep(batch.size, nbatches)))
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  p.choose.1 <- unlist(lapply(seq_len(nbatches),
                              function(x, df, fav.opt, batch.int){
                                mean(df$selection[fav.opt >= batch.int[x] &
                                                    fav.opt < batch.int[x+1]] == 1,
                                     na.rm = TRUE)
                              },
                              df = df, fav.opt = fav.opt, batch.int = batch.int))
  
  numobs <- unlist(lapply(seq_len(nbatches),
                          function(x, df, fav.opt, batch.int){
                            vec <- df$selection[fav.opt >= batch.int[x] &
                                                  fav.opt < batch.int[x+1]]
                            vec <- vec[!is.na(vec)]
                            
                            lvec <- length(vec)
                            
                            lvec
                            
                          },
                          df = df, fav.opt = fav.opt, batch.int = batch.int))
  
  # scale the size ob points
  total.l <- length(df$selection[!is.na(df$selection)])
  
  numobs.s <- round(numobs / total.l, 2)
  

  # plot the results
  plot(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = "b",
       ylab = "p of Choosing Option 1", col = gray(0.3, .7), lwd = 2, cex = exp(numobs.s),
       cex.lab = 1.5, cex.axis = 1.5, ylim = c(0, 1), pch = 16, ...)
  
  text(x = batch.int[1:(length(batch.int)-1)] + (batch.size / 2), 
       y = p.choose.1, 
       labels = numobs.s, pos = 3)
  
  df.evidence <- data.frame("p.choose.1" = p.choose.1,
                            "batch.int" = batch.int[1:(length(batch.int)-1)] + (batch.size / 2),
                            "n.obs" = numobs,
                            "n.obs.s" = numobs.s)
  
  df.evidence
}

### Evidence in RSF strategy --------------
RSF.all <- evidence_strength(subset(df.trial, goal.condition == "Goal"), "RSF", 2,
                             xlab = "p getthere 1 - p getthere 2",
                             main = "Evidence Strength RSF, All Trials, Goal Condition")
RSF.ug <- evidence_strength(subset(df.trial, goal.condition == "Goal" & overGoal == 0), "RSF", 2,
                            xlab = "p getthere 1 - p getthere 2",
                            main = "Evidence Strength RSF, Under Goal, Goal Condition")

# with the evidence2 function
RSF.all2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal"), "RSF", 2, 10,
                             xlab = "p getthere 1 - p getthere 2",
                             main = "Evidence Strength RSF, All Trials, Goal Condition")
RSF.ug2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal" & overGoal == 0), "RSF", 2,
                            10, xlab = "p getthere 1 - p getthere 2",
                            main = "Evidence Strength RSF, Under Goal, Goal Condition")

# separated for variance conditions
RSF.equal2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal" &
                                          variance.condition == "Equal"), "RSF", 2,
                              10, xlab = "p getthere 1 - p getthere 2",
                              main = "Evidence Strength RSF, Equal EVs, Goal Condition")

RSF.low2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal" &
                                          variance.condition == "Low"), "RSF", 2,
                                 10, xlab = "p getthere 1 - p getthere 2",
                                 main = "Evidence Strength RSF, Low Variance, Goal Condition")

RSF.high2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal" &
                                        variance.condition == "High"), "RSF", 2,
                               10, xlab = "p getthere 1 - p getthere 2",
                               main = "Evidence Strength RSF, High Variance, Goal Condition")

### Evidence in EV strategy ---------------

# THE FOLLOWING COMMENTED-OUT CODE ONLY HAD TO BE RUN ONCE, I THEN SAVED IT 
# # we first need to calculate the subjective sds
# 
# # prepare data
# df.trial$sd.sub1 <- NA
# df.trial$sd.sub2 <- NA
# 
# for (xx in seq_along(df.trial$id)){
#   m1.vec <- df.trial$outcome[df.trial$id == df.trial$id[xx] &
#                               df.trial$game == df.trial$game[xx] &
#                               df.trial$trial < df.trial$trial[xx] &
#                               df.trial$selection == 1]
#   
#   m2.vec <- df.trial$outcome[df.trial$id == df.trial$id[xx] &
#                               df.trial$game == df.trial$game[xx] &
#                               df.trial$trial < df.trial$trial[xx] &
#                               df.trial$selection == 2]
#   
#   # compute sds for this vector
#   
#   sd1 <- ifelse(length(m1.vec) > 1, sd(m1.vec, na.rm = TRUE), 1)
#   sd2 <- ifelse(length(m2.vec) > 1, sd(m2.vec, na.rm = TRUE), 1)
#   
#   # append to df
#   df.trial$sd.sub1[xx] <- sd1
#   df.trial$sd.sub2[xx] <- sd2
# }

# note that results only look interpretable when we round to 0 or at most 1 digit after comma
EV.goal <- evidence_strength(subset(df.trial, goal.condition == "Goal"), "EV", 0,
                             xlab = "d prime 1 - d prime 2",
                             main = "Evidence Strength EV, All Trials, Goal Condition")

EV.goal.ug <- evidence_strength(subset(df.trial, goal.condition == "Goal" & overGoal == 0), "EV", 0,
                            xlab = "d prime 1 - d prime 2",
                            main = "Evidence Strength EV, Under Goal, Goal Condition")

EV.no.goal <- evidence_strength(subset(df.trial, goal.condition == "NoGoal"), "EV", 0,
                             xlab = "d prime 1 - d prime 2",
                             main = "Evidence Strength EV, All Trials, No Goal Condition")

# with the evidence2 function
EV.goal2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal"), "EV", 0, 10,
                             xlab = "d prime 1 - d prime 2",
                             main = "Evidence Strength EV, All Trials, Goal Condition")

EV.goal.ug2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal" & overGoal == 0), "EV", 0,
                                10, xlab = "d prime 1 - d prime 2",
                                main = "Evidence Strength EV, Under Goal, Goal Condition")

EV.no.goal2 <- evidence_strength2(subset(df.trial, goal.condition == "NoGoal"), "EV", 0, 10,
                                xlab = "d prime 1 - d prime 2",
                                main = "Evidence Strength EV, All Trials, No Goal Condition")

# let's check the different variance conditions separately

EV.goal.equal2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal" &
                                              variance.condition == "Equal"), "EV", 0, 10,
                               xlab = "d prime 1 - d prime 2",
                               main = "Evidence Strength EV, Equal EVs, Goal Condition")

EV.goal.low2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal" &
                                            variance.condition == "Low"), "EV", 0, 10,
                                     xlab = "d prime 1 - d prime 2",
                                     main = "Evidence Strength EV, Low variance, Goal Condition")

EV.goal.high2 <- evidence_strength2(subset(df.trial, goal.condition == "Goal" &
                                             variance.condition == "High"), "EV", 0, 10,
                                     xlab = "d prime 1 - d prime 2",
                                     main = "Evidence Strength EV, High Variance, Goal Condition")



EV.no.goal.equal.2 <- evidence_strength2(subset(df.trial, goal.condition == "NoGoal" &
                                                  variance.condition == "Equal"), "EV", 0, 10,
                                  xlab = "d prime 1 - d prime 2",
                                  main = "Evidence Strength EV, Equal EVs, No Goal Condition")

EV.no.goal.low.2 <- evidence_strength2(subset(df.trial, goal.condition == "NoGoal" &
                                                  variance.condition == "Low"), "EV", 0, 10,
                                         xlab = "d prime 1 - d prime 2",
                                         main = "Evidence Strength EV, Low Variance, No Goal Condition")

EV.no.goal.high.2 <- evidence_strength2(subset(df.trial, goal.condition == "NoGoal" &
                                                  variance.condition == "High"), "EV", 0, 10,
                                         xlab = "d prime 1 - d prime 2",
                                         main = "Evidence Strength EV, Equal EVs, No Goal Condition")

