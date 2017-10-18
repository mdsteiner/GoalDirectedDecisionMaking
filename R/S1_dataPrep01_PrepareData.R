rm(list = ls())
gc()

# ------------
# DATA CLEANING
#
# Study (working title): Need based decision making in a reinforcement learning task.
# 
# Authors:
#   - Markus D. Steiner (markus.d.steiner@gmail.com)
#   - Nathaniel D. Phillips (Nathaniel.D.Phillips.is@gmail.com)
# 
# This script anonymizes, separates and prepares the data. Note that the data for section A and B of
# this script will not be provided, because it contains sensible information whith which participants 
# could be identified. Therefore only the anonymized data obtained with this script will be made available.
# 
# Code Sections:
#   0: Load Libraries
#   A: Check Exclusion Criteria and Prepare Anonymization
#   B: Anonymize and Separate Data
#   C: Prepare Data
#     C1: Game Data
#        C1.1: Trial Level Data Frame
#        C1.2: Game Level Data Frame
#        C1.3: Participant Level Data Frame
#     C2: Survey Data
# ------------

# --------------------------
# Section 0: Load Libraries
# --------------------------

if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
source("r/learning_functions.R")

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())


# --------------------------------------------------------------
# Section A: Check Exclusion Criteria and Prepare Anonymization
# --------------------------------------------------------------

# read in files with ids of persons who were not allowed to participate but participated anyway
exclude.df <- read.table("data/Study1Data/idsNotAllowed.txt", header = TRUE, as.is = TRUE)

exclude1 <- tolower(exclude.df[,1])

# get a list of the id files available
fil <- list.files("data/Study1Data/ids/", full.names = TRUE)

## read in the id files and save the ids in a matrix

# prepare matrix
ids <- matrix(NA, ncol = 1, nrow = length(fil))

# read in files and save in matrix
for (ii in 1:length(fil)){
  
  temp.df <- read.table(fil[ii], header = T, sep = ",")
  
  ids[ii, 1] <- as.character(temp.df[1, 1])
  
}

# get the ids of those who either only accessed one part of the study or who experienced a crash
table.ids <- table(ids)

# ids to exclude
exclude <- tolower(names(table.ids[table.ids != 2]))

# only use the ones not already in exclude 1
exclude <- exclude[-which(exclude %in% exclude1)]

# vector to index ids
unique.ids <- unique(ids)

# create new id variable
anonym.ids <- NULL

for (kk in 1:length(unique.ids)){
  
  anonym.ids <- c(anonym.ids, paste0("id_", ifelse(kk < 10, "00", ifelse(kk < 100, "0", "")), kk))
  
}

# write in file to have a report of how many participants are excluded at which state

# read what's already in the file
fileConnRead <- file("documents/exclution_log.txt", "r")
txt <- readLines(fileConnRead, warn = F)
close(fileConnRead)

# add old and new lines
fileConnWrite <- file("documents/exclution_log.txt", "wt")
write(c(txt, "\n", paste("Number of participants excluded because they were not allowed to participate:",
                         length(exclude1)), "Their Ids are:", paste(
                           anonym.ids[which(tolower(unique.ids) %in% exclude1)], collapse = "; "), "\n\n",
        paste("Number of participant excluded due to too many or too few files are:",
                   length(exclude)), "Their Ids are:", paste(
                     anonym.ids[which(tolower(unique.ids) %in% exclude)], collapse = "; ")),
      fileConnWrite)
close(fileConnWrite)

exclude1 <- c(exclude1, exclude)

# get a list of the data files and directory they're in
list.games <- list.files("data/Study1Data/data/", pattern = "_g.csv", full.names = T)
list.surveys <- list.files("data/Study1Data/data/", pattern = "_s.csv", full.names = T)

# prepare vector for ids to exclude because of certain behaviroal patterns
exclude.behavior <- NULL
exclude.behavior.anonym <- NULL

# loop through game files to check for exclusion criteria in behavioral patterns
for (ii in 1:length(list.games)){
  
  # read in files
  temp.game <- read.table(list.games[ii], header = T, sep = ",", as.is = T)
  
  # check response times for exclusion criteria
  mean.max.time <- mean(temp.game$time > 2, na.rm = T)
  
  mean.min.time <- mean(temp.game$time < 0.55, na.rm = T)
  
  # check for switch rates
  m.switches <- NULL
  
  # ignore practice game, thus start at 1
  for (gam in 2:max(temp.game$game)){
    
    m.switches <- c(m.switches, mean(temp.game$selection[temp.game$game == gam] == 1))
    
  }
  
  games.never.switched <- sum(m.switches %in% c(0,1))
  
  check.fails <- temp.game$checkFails[1]
  
  
  # check if an exclusion criterion was met and if so, save id
  
  if (mean.max.time >= .8 | mean.min.time >= .8 | games.never.switched >= 5 | check.fails >= 2){
    
    exclude.behavior <- c(exclude.behavior, tolower(as.character(temp.game$workerid[1])))
    
    exclude.behavior.anonym <- c(exclude.behavior.anonym,
                                 anonym.ids[which(tolower(unique.ids) %in%
                                                    tolower(as.character(temp.game$workerid[1])))])
    
  }
  
}

# update log and exclusion files
if (length(exclude.behavior.anonym) > 0){
  
  # write in file to have a report of how many participants are excluded at which state
  
  # read what's already in the file
  fileConnRead <- file("documents/exclution_log.txt", "r")
  txt <- readLines(fileConnRead, warn = F)
  close(fileConnRead)
  
  # add old and new lines
  fileConnWrite <- file("documents/exclution_log.txt", "wt")
  write(c(txt, "\n", paste("Number of participant excluded due to behavioral patterns in switch rates and response times are:",
                     length(exclude.behavior.anonym)), "Their Ids are:", paste(
                       anonym.ids[which(tolower(unique.ids) %in% exclude.behavior)], collapse = "; ")),
        fileConnWrite)
  close(fileConnWrite)
  
  # update exclude1 vector
  exclude1 <- c(exclude1, exclude.behavior)

}

# prepare vector for ids to exclude because of certain answers in the survey
exclude.survey <- NULL
exclude.survey.anonym <- NULL

# loop through survey files to check for exclusion criteria in behavioral patterns
for (jj in 1:length(list.surveys)){
  
  # read in files
  temp.surveys <- read.table(list.surveys[jj], header = T, sep = ",", as.is = T)
  
  # check if an exclusion criterion was met and if so, save id
  
  if (temp.surveys$trust.data == 2){
    
    exclude.survey <- c(exclude.survey, tolower(as.character(temp.surveys$workerid[1])))
    
    exclude.survey.anonym <- c(exclude.survey.anonym,
                                 anonym.ids[which(tolower(unique.ids) %in%
                                                    tolower(as.character(temp.surveys$workerid[1])))])
    
  }
  
}

# update log and exclusion files
if (length(exclude.survey.anonym) > 0){
  
  # write in file to have a report of how many participants are excluded at which state
  
  # read what's already in the file
  fileConnRead <- file("documents/exclution_log.txt", "r")
  txt <- readLines(fileConnRead, warn = F)
  close(fileConnRead)
  
  # add old and new lines
  fileConnWrite <- file("documents/exclution_log.txt", "wt")
  write(c(txt, "\n", paste("Number of participant excluded due to their answer in the survey, that we cannot trust their data is:",
                     length(exclude.survey.anonym)), "Their Ids are:", paste(
                       anonym.ids[which(tolower(unique.ids) %in% exclude.survey)], collapse = "; ")),
        fileConnWrite)
  close(fileConnWrite)
  
  # update exclude1 vector
  exclude1 <- c(exclude1, exclude.survey)
  
}


# --------------------------------------
# Section B: Anonymize and Separate Data
# --------------------------------------

# loop through game files
for (ii in 1:length(list.games)){
  
  # read in files
  temp.game <- read.table(list.games[ii], header = T, sep = ",", as.is = T)
  
  # determine where to save the data (either folder with erroneous data or with useful data)
  if (tolower(as.character(temp.game$workerid[1])) %in% exclude1){
    
    folder <- "data/Study1Data/errorData/"
    
  } else {
    
    folder <- "data/Study1Data/useData/"
    
  }
  
  # change workerid to a number
  id.i <- anonym.ids[which(tolower(unique.ids) %in% tolower(as.character(temp.game$workerid[1])))]
  temp.game$workerid <- id.i
    
  # save the file as .txt file
  write.table(temp.game, paste0(folder,
                                id.i, "_g_S1.txt"),
              row.names = F, sep = "\t")

}

# loop through survey files
for (jj in 1:length(list.surveys)){
  
  # read in files
  temp.surveys <- read.table(list.surveys[jj], header = T, sep = ",", as.is = T)
  
  # determine where to save the data (either folder with erroneous data or with useful data)
  if (tolower(as.character(temp.surveys$workerid[1])) %in% exclude1){
    
    folder <- "data/Study1Data/errorData/"
    
  } else {
    
    folder <- "data/Study1Data/useData/"
    
  }
  
  # change workerid to a number
  id.i <- anonym.ids[which(tolower(unique.ids) %in% tolower(as.character(temp.surveys$workerid[1])))]
  temp.surveys$workerid <- id.i
  
  # get rid of condition columns, with potential personal information
  temp.surveys <- temp.surveys[, !(names(temp.surveys) %in% c("completion.code",
                                                              "comments"))]
  
  # save the file as .rds file
  write.table(temp.surveys, paste0(folder,
                                   id.i, "_s_S1.txt"),
              row.names = F, sep = "\t")

}

# ------------------------
# Section C: Prepare Data
# ------------------------

# get list of data files
list.games <- list.files("data/Study1Data/useData/", pattern = "_g_S1.txt", full.names = T)
list.surveys <- list.files("data/Study1Data/useData/", pattern = "_s_S1.txt", full.names = T)


### Section C1: Game Data

## Section C1.1: Trial Level Data


# read in the first data frame to have a reference
temp.game <- read.table(list.games[1], header = T, sep = "\t", as.is = T)

# prepare an array to store the data in
all.game.data <- array(NA, c(nrow(temp.game), 11, length(list.games)))

# prepare the indices that will be used to get the selected option
char.index <- c(1, 3)

# loop through game files
for (ii in 1:length(list.games)){
  
  # read in files
  temp.game <- read.table(list.games[ii], header = T, sep = "\t", as.is = T)
  
  # save the selected option (distribution 1, 2)
  select.opt <- substr(temp.game$option.order, char.index[temp.game$selection], char.index[temp.game$selection])
  temp.game$select.opt <- select.opt
  
  # write the data in one plane of the array
  all.game.data[,1,ii] <- as.character(temp.game$workerid[1])
  all.game.data[,2,ii] <- temp.game$select.opt
  all.game.data[,3,ii] <- temp.game$trial
  all.game.data[,4,ii] <- temp.game$game
  all.game.data[,5,ii] <- temp.game$outcome
  all.game.data[,6,ii] <- temp.game$condition
  all.game.data[,7,ii] <- temp.game$time
  all.game.data[,8,ii] <- temp.game$points.cum
  all.game.data[,9,ii] <- temp.game$n.goals.reached
  all.game.data[,10,ii] <- temp.game$checkFails
  all.game.data[,11,ii] <- temp.game$which.high.ev
}

# prepare vectors
select <- NULL
switch <- NULL
condit <- NULL
trials <- NULL
point.cum <- NULL
games <- NULL
workerids <- NULL
outcomes <- NULL
times <- NULL
goalReached <- NULL
n.goals.reached <- NULL
checkFails <- NULL
which.high.ev <- NULL

# loop through game data
for (xx in 1:length(list.games)){
  
  # loop through games in the data
  for (gam in 1:max(temp.game$game)){
    
    # store relevant data to later on bind everything to a long dataframe
    temp.select <- all.game.data[,2,xx][all.game.data[,4,xx] == gam]
    temp.switch <- NA
    condi <- all.game.data[1,6,xx]
    temp.cond <- condi
    points.cum.temp <- all.game.data[,8,xx][all.game.data[,4,xx] == gam]
    trial.temp <- all.game.data[,3,xx][all.game.data[,4,xx] == gam]
    outcome.temp <- all.game.data[,5,xx][all.game.data[,4,xx] == gam]
    gam.temp <- gam
    workerid.s <- as.character(all.game.data[1, 1, xx])
    workerid.temp <- workerid.s
    n.goals.reached.s <- all.game.data[1, 9, xx]
    n.goals.reached.temp <- n.goals.reached.s
    checkFails.s <- all.game.data[1, 10, xx]
    checkFails.temp <- checkFails.s
    which.high.ev.s <- all.game.data[1, 11, xx]
    which.high.ev.temp <- which.high.ev.s
    time.temp <- all.game.data[,7,xx][all.game.data[,4,xx] == gam]
    goal.reached.i <- ifelse(as.numeric(points.cum.temp[length(points.cum.temp)]) >= 100, 1, 0)
    goal.reached.temp <- goal.reached.i
    
    # loop through trials in a game
    for (sel in 2:length(temp.select)){
      temp.switch <- c(temp.switch, ifelse(temp.select[sel-1] == temp.select[sel], 0, 1)) # 1 for switch
      temp.cond <- c(temp.cond, condi)
      gam.temp <- c(gam.temp, gam)
      workerid.temp <- c(workerid.temp, workerid.s)
      goal.reached.temp <- c(goal.reached.temp, goal.reached.i)
      n.goals.reached.temp <- c(n.goals.reached.temp, n.goals.reached.s)
      checkFails.temp <- c(checkFails.temp, checkFails.s)
      which.high.ev.temp <- c(which.high.ev.temp, which.high.ev.s)
      
    }
    
    # append the data to the precreated vectors
    select <- c(select, temp.select)
    switch <- c(switch, temp.switch)
    condit <- c(condit, temp.cond)
    trials <- c(trials, trial.temp)
    point.cum <- c(point.cum, points.cum.temp)
    games <- c(games, gam.temp)
    workerids <- c(workerids, workerid.temp)
    outcomes <- c(outcomes, outcome.temp)
    times <- c(times, time.temp)
    goalReached <- c(goalReached, goal.reached.temp)
    n.goals.reached <- c(n.goals.reached, n.goals.reached.temp)
    checkFails <- c(checkFails, checkFails.temp)
    which.high.ev <- c(which.high.ev, which.high.ev.temp)
    
  }
}

# create a dataframe in long format
df.long <- data.frame(workerids, games, condit, select, switch, trials, outcomes,
                      point.cum, times, goalReached, n.goals.reached, checkFails, which.high.ev)

# rename things
names(df.long)
names(df.long) <- c("id", "game", "condition", "selection", "switched", "trial",
                    "outcome", "points.cum", "resp.time", "goalReached", "nGoalsReached", "checkFails", "whichHighEV")

df.long$id <- as.character(df.long$id)
# create new variable whether participants were, in a given trial, over the goal (1) or not (0)
df.long$overGoal <- NA
df.long$overGoal <- ifelse(as.numeric(as.character(df.long$points.cum)) >= 100, 1, 0)

# create new variable whether participants in a given trial chose the high variance option
df.long$high.var.chosen <- ifelse(df.long$selection == "2", 1, 0)

# create new variable whether participants in a given trial chose the high EV option
df.long$highEV <- NA

# in condition 1 and 4 both have the same ev
df.long$highEV[df.long$condition %in% c(1, 4)] <- 1

# in condiiton 2 and 5, the first option has the higher ev
df.long$highEV[df.long$condition %in% c(2, 5)] <- ifelse(df.long$selection[df.long$condition %in% c(2, 5)] == "1", 1, 0)

# in condition 3 and 6, the second option has the higher ev
df.long$highEV[df.long$condition %in% c(3, 6)] <- ifelse(df.long$selection[df.long$condition %in% c(3, 6)] == "2", 1, 0)

# make sure the variables are saved in the proper class
df.long$outcome <- as.numeric(as.character(df.long$outcome))
df.long$resp.time <- as.numeric(as.character(df.long$resp.time))
df.long$trial <- as.numeric(as.character(df.long$trial))
df.long$points.cum <- as.numeric(as.character(df.long$points.cum))

# rename conditions
# condition 1/4 = "NE"/"GE" = No goal, equal ev/ goal, equal ev
# condition 2/5 = "NL"/"GL" = No goal, high ev is LOW variance/ goal, high ev is LOW variance
# condition 3/6 = "NH"/"GH" = No goal, high ev is HIGH variance/ goal, high ev is HIGH variance
name.vec <- c("NE", "NL", "NH", "GE", "GL", "GH")
goal.vec <- rep(c("NoGoal", "Goal"), each = 3)
variance.vec <- rep(c("Equal", "Low", "High"), 2)

df.long$goal.condition <- goal.vec[as.numeric(df.long$condition)]
df.long$variance.condition <- variance.vec[as.numeric(df.long$condition)]
df.long$condition <- name.vec[as.numeric(df.long$condition)]

# create empty vectors
o1.vec <- NULL
o2.vec <- NULL

o1.vec.sub <- NULL
o2.vec.sub <- NULL

sub.mean1 <- NULL
sub.mean2 <- NULL

sd.sub1 <- NULL
sd.sub2 <- NULL

# these are the means and sds of the distributions of the two environments
mu.mat <- matrix(c(4, 4, 4, 2.5, 2.5, 4), ncol = 2, byrow = T)
sd.mat <- matrix(rep(c(2.5, 11), 3), ncol = 2, byrow = T)

# loop through trials
for (xx in 1:nrow(df.long)){

  # for both options compute the probability of reaching the 
  # goal if this option was chosen for the rest of the game
  
  # change from preregistration: only use last trials to determine points needed.
  if (df.long$trial[xx] == 1){
    points.needed.i <- ifelse(df.long$game[xx] == 1, 25, 100)
  } else {
    points.needed.i <- ifelse(df.long$game[xx] == 1, 25, 100) - df.long$points.cum[xx-1]
  }
  trials.left.i <- 26 - df.long$trial[xx] # change from preregistration: max trials + 1
  
  if (df.long$condition[xx] %in% c(1,4)){
    
    # same EV environment
    p.temp <- p.getthere.fun(points.needed = points.needed.i,
                             trials.left = trials.left.i,
                             mu = mu.mat[1,],
                             sigma = sd.mat[1,])
    
  } else if (df.long$condition[xx] %in% c(2,5)) {
    
    # high EV is low vatriance environment
    p.temp <- p.getthere.fun(points.needed = points.needed.i,
                             trials.left = trials.left.i,
                             mu = mu.mat[2,],
                             sigma = sd.mat[2,]) 
    
  } else {
    
    # high EV is high variance environment
    p.temp <- p.getthere.fun(points.needed = points.needed.i,
                             trials.left = trials.left.i,
                             mu = mu.mat[3,],
                             sigma = sd.mat[3,])
    
  }
    
  
  # append probability values to vector
  o1.vec <- c(o1.vec, p.temp[1])
  o2.vec <- c(o2.vec, p.temp[2])

  
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
  m1 <- ifelse(length(m1.vec) > 0, mean(m1.vec, na.rm = TRUE), 0)
  m2 <- ifelse(length(m2.vec) > 0, mean(m2.vec, na.rm = TRUE), 0)
  
  sd1 <- ifelse(length(m1.vec) > 1, sd(m1.vec, na.rm = TRUE), 1)
  sd2 <- ifelse(length(m2.vec) > 1, sd(m2.vec, na.rm = TRUE), 1)
  
  # append to a vector to use later
  sub.mean1 <- c(sub.mean1, m1)
  sub.mean2 <- c(sub.mean2, m2)
  
  ## change from preregistered code : append sds
  # append to a vector to use later
  sd.sub1 <- c(sd.sub1, sd1)
  sd.sub2 <- c(sd.sub2, sd2)
  
  # compute probabilities with these subjective distributions
  mu.i <- c(m1, m2)
  sigma.i <- c(sd1, sd2)
  
  p.temp.subj <- p.getthere.fun(points.needed = points.needed.i,
                           trials.left = trials.left.i,
                           mu = mu.i,
                           sigma = sigma.i)
  
  o1.vec.sub <- c(o1.vec.sub, p.temp.subj[1])
  o2.vec.sub <- c(o2.vec.sub, p.temp.subj[2])
  
}

# the probabilities were calculated with complete information of points in a trial
# i.e. the obtained probability is the one for the next trial.

df.long$p.getthere.1 <- o1.vec
df.long$p.getthere.2 <- o2.vec

df.long$p.getthere.1.subj <- o1.vec.sub
df.long$p.getthere.2.subj <- o2.vec.sub

# append subjective mean vectors
df.long$subj.mean.1 <- sub.mean1
df.long$subj.mean.2 <- sub.mean2

# append subective sd vectors
df.long$sd.sub1 <- sd.sub1
df.long$sd.sub2 <- sd.sub2

# prepare objects
choose.highvar <- NULL
choose.highvar.subj <- NULL
pred.EV <- NULL
pred.RSF <- NULL

# for objective and subjective p.getthere values, create vectors containing 1, when the p.getthere value for the 
# high variance option is higher than for the low variance option, and 0 otherwise
for (jj in 1:nrow(df.long)){

    # check if it was rational to choose the high variance obtion
    choose.highvar <- c(choose.highvar, ifelse(df.long$p.getthere.2[jj] > df.long$p.getthere.1[jj], 1, 0))
    
    choose.highvar.subj <- c(choose.highvar.subj,
                             ifelse(df.long$p.getthere.2.subj[jj] > df.long$p.getthere.1.subj[jj], 1, 0))
    
    pred.EV <- c(pred.EV, ifelse(df.long$subj.mean.1[jj] > df.long$subj.mean.2[jj], 1,
                                 ifelse(df.long$subj.mean.1[jj] == df.long$subj.mean.2[jj], sample(1:2, 1), 2)))
    
    pred.RSF <- c(pred.RSF, ifelse(df.long$p.getthere.1.subj[jj] > df.long$p.getthere.2.subj[jj], 1,
                                 ifelse(df.long$p.getthere.1.subj[jj] == df.long$p.getthere.2.subj[jj], sample(1:2, 1), 2)))

}

# append to dataframe
df.long$choose.highvar <- choose.highvar
df.long$choose.highvar.subj <- choose.highvar.subj
df.long$pred.EV <- pred.EV
df.long$pred.RSF <- pred.RSF
df.long$pred.EV.acc <- ifelse(df.long$selection == df.long$pred.EV, 1, 0)
df.long$pred.RSF.acc <- ifelse(df.long$selection == df.long$pred.RSF, 1, 0)

# save trial level dataframe
saveRDS(df.long, "data/Study1Data/useData/S1_dataTrialLevel.rds")

## Section C1.2: Game Level Data

# aggregate important variables to game level

df.game <- df.long %>%
  filter(game > 1) %>%
  group_by(id, game, condition, goal.condition, variance.condition) %>%
  summarise(
    switch.rate = mean(switched, na.rm = TRUE),
    points.cum = max(points.cum),
    resp.time.median = median(resp.time, na.rm = TRUE),
    goalReachedRate = max(as.numeric(as.character(goalReached))),
    nGoalsReached = max(as.numeric(as.character(nGoalsReached))),
    checkFails = max(as.numeric(as.character(checkFails))),
    whichHighEV = max(as.numeric(as.character(whichHighEV))),
    overGoal.rate = mean(overGoal, na.rm = TRUE),
    high.var.chosen.rate = mean(high.var.chosen, na.rm = TRUE),
    highEV.rate = mean(highEV, na.rm = TRUE),
    p.getthere.1.mean = mean(p.getthere.1, na.rm = TRUE),
    p.getthere.2.mean = mean(p.getthere.2, na.rm = TRUE),
    p.getthere.1.subj.mean = mean(p.getthere.1.subj, na.rm = TRUE),
    p.getthere.2.subj.mean = mean(p.getthere.2.subj, na.rm = TRUE),
    subj.mean.1.end = subj.mean.1[length(subj.mean.1)],
    subj.mean.2.end = subj.mean.2[length(subj.mean.2)],
    choose.highvar.rate = mean(choose.highvar, na.rm = TRUE),
    choose.highvar.subj.rate = mean(choose.highvar.subj, na.rm = TRUE),
    pred.EV.acc.rate = mean(pred.EV.acc, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc, na.rm = TRUE)
  )

risky.ag <- NULL
risky.ug <- NULL

for (nn in unique(df.long$id)){
  
  for (gg in 2:max(df.long$game)){
  
    risky.ag <- c(risky.ag,
                  mean(df.long$selection[df.long$id == nn & df.long$game == gg & df.long$points.cum >= ifelse(gg == 1, 25, 100)]
                     == 2, na.rm = TRUE))
    risky.ug <- c(risky.ug,
                  mean(df.long$selection[df.long$id == nn & df.long$game == gg & df.long$points.cum < ifelse(gg == 1, 25, 100)]
                     == 2, na.rm = TRUE))
  }
  
}

df.game$risky.ag <- risky.ag
df.game$risky.ug <- risky.ug

saveRDS(df.game, "data/Study1Data/useData/S1_dataGameLevel.rds")

## Section C1.3: Participant Level Data

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
    pred.RSF.acc.rate = mean(pred.RSF.acc, na.rm = TRUE)
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


saveRDS(df.part, "data/Study1Data/useData/S1_dataParticipantLevel.rds")



