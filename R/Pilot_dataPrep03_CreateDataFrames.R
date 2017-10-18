rm(list = ls())
gc()
# ---------------------------
# Script to create dataframes from the separate individual data files
# ---------------------------


# get list of data files
# list.games <- list.files("data/", pattern = "_g.rds")
# list.surveys <- list.files("data/", pattern = "_s.rds")
list.games <- list.files("data/", pattern = "_g.txt")
list.surveys <- list.files("data/", pattern = "_s.txt")


list.games <- paste0("data/", list.games)
list.surveys <- paste0("data/", list.surveys)



# -------------------
# Trial Level Dataframe
# -------------------

# here be dragons



# read in the first data frame to have a reference
#temp.game <- readRDS(list.games[1])
temp.game <- read.table(list.games[1], header = T, sep = "\t", as.is = T)

# delete the data rows that don't contain usable data (they were
# necessary to create in the experiment code)
temp.game <- temp.game[-which(temp.game$selection == 0),]

# prepare an array to store the data in
all.game.data <- array(NA, c(nrow(temp.game), 8, length(list.games)))

# prepare the indices that will be used to get the selected option
char.index <- c(1, 3, 5)

# loop through game files
for (ii in 1:length(list.games)){
  
  # read in files
  #temp.game <- readRDS(list.games[ii])
  temp.game <- read.table(list.games[ii], header = T, sep = "\t", as.is = T)
  
  # save the selected option (distribution 1, 2 or 3)
  select.opt <- ifelse(temp.game$selection == 0, "0", substr(temp.game$option.order,
                                                             char.index[temp.game$selection],
                                                             char.index[temp.game$selection]))
  temp.game$select.opt <- select.opt
  
  # delete the data rows that don't contain usable data (they were
  # necessary to create in the experiment code)
  temp.game <- temp.game[-which(temp.game$select.opt == "0"),]
  
  # write the data in one plane of the array
  all.game.data[,1,ii] <- as.character(temp.game$workerid[1])
  all.game.data[,2,ii] <- temp.game$select.opt
  all.game.data[,3,ii] <- temp.game$trial
  all.game.data[,4,ii] <- temp.game$game
  all.game.data[,5,ii] <- temp.game$outcome
  all.game.data[,6,ii] <- temp.game$condition
  all.game.data[,7,ii] <- temp.game$time
  all.game.data[,8,ii] <- temp.game$points.cum
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

# loop through game data
for (xx in 1:length(list.games)){
  
  # loop through games in the data
  for (gam in 1:6){
    
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
    time.temp <- all.game.data[,7,xx][all.game.data[,4,xx] == gam]
    goal.reached.i <- ifelse(condi %in% 3:4,
                             ifelse(points.cum.temp[length(points.cum.temp)] >= 135, 1, 0), NA)
    goal.reached.temp <- goal.reached.i
    
    # loop through trials in a game
    for (sel in 2:length(temp.select)){
      temp.switch <- c(temp.switch, ifelse(temp.select[sel-1] == temp.select[sel], 0, 1)) # 1 for switch
      temp.cond <- c(temp.cond, condi)
      gam.temp <- c(gam.temp, gam)
      workerid.temp <- c(workerid.temp, workerid.s)
      goal.reached.temp <- c(goal.reached.temp, goal.reached.i)
      
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
    
  }
}

# create a dataframe in long format
df.long <- data.frame(workerids, games, condit, select, switch, trials, outcomes,
                      point.cum, times, goalReached)

# rename things
names(df.long)
names(df.long) <- c("workerid", "game", "condition", "selection", "switched", "trial",
                    "outcome", "points.cum", "resp.time", "goalReached")

df.long$workerid <- as.character(df.long$workerid)
# create new variable whether participants were, in a given trial, over the goal (1) or not (0)
df.long$overGoal <- NA
df.long$overGoal <- ifelse(df.long$condition %in% 3:4 &
                             as.numeric(as.character(df.long$points.cum)) >= 135, 1, 0)

# create new variable whether participants in a given trial chose the high variance option
df.long$high.var.chosen <- ifelse(df.long$condition %in% c(1,3),
                                  ifelse(df.long$selection == "3", 1, 0),
                                  ifelse(df.long$selection == "1", 1, 0))

# create new variable whether participants in a given trial chose the high EV option
df.long$highEV <- ifelse(df.long$selection == "1", 1, 0)

df.long$outcome <- as.numeric(as.character(df.long$outcome))
df.long$resp.time <- as.numeric(as.character(df.long$resp.time))
df.long$trial <- as.numeric(as.character(df.long$trial))
df.long$points.cum <- as.numeric(as.character(df.long$points.cum))

saveRDS(df.long, "data/dataTrialLevel.rds")

# -------------------
# Game Level Dataframe
# -------------------

# aggregate important variables to game level
# again, here be dragons...


aa.switch <- aggregate(formula = switched ~ workerid + game, FUN = mean, data = df.long)
aa.outcome.mean <- aggregate(formula = outcome ~ workerid + game, FUN = mean, data = df.long)
aa.outcome.sum <- aggregate(formula = outcome ~ workerid + game, FUN = sum, data = df.long)
aa.resp.time <- aggregate(formula = resp.time ~ workerid + game, FUN = median, data = df.long)
aa.high.var.chosen <- aggregate(formula = high.var.chosen ~ workerid + game, FUN = mean,
                                data = df.long)
aa.highEV <- aggregate(formula = highEV ~ workerid + game, FUN = mean, data = df.long)

df.game <- data.frame("workerid" = as.character(aa.switch$workerid),
                      "game" = aa.switch$game,
                      "mean.switched" = aa.switch$switched,
                      "outcome.mean" = aa.outcome.mean$outcome,
                      "outcome.sum" = aa.outcome.sum$outcome,
                      "resp.time.median" = aa.resp.time$resp.time,
                      "high.var.chosen.mean" = aa.high.var.chosen$high.var.chosen,
                      "highEV.mean" = aa.highEV$highEV)

df.game$workerid <- as.character(df.game$workerid)
df.game$condition <- NA

for (kk in 1:nrow(df.game)){
  df.game$condition[kk] <- df.long$condition[df.long$workerid == df.game$workerid[kk]][1]
}


df.game$goalReached <- ifelse(df.game$condition %in% 3:4,
                              ifelse(df.game$outcome.sum >= ifelse(df.game$game == 1, 30, 135),
                                     1, 0), NA)

saveRDS(df.game, "data/dataGameLevel.rds")

# ------------------
# Participant Level Dataframe
# ------------------

# create a participant level dataframe

# aggregate data to participant level
a.switch <- aggregate(formula = switched ~ workerid, FUN = mean, data = df.long)
a.outcome.mean <- aggregate(formula = outcome ~ workerid, FUN = mean, data = df.long)
a.outcome.sum <- aggregate(formula = outcome ~ workerid, FUN = sum, data = df.long)
a.resp.time <- aggregate(formula = resp.time ~ workerid, FUN = median, data = df.long)
a.high.var.chosen <- aggregate(formula = high.var.chosen ~ workerid, FUN = mean, data = df.long)
a.highEV <- aggregate(formula = highEV ~ workerid, FUN = mean, data = df.long)

# create the dataframe
df.participant <- data.frame("workerid" = as.character(a.switch$workerid),
                             "mean.switched" = a.switch$switched,
                             "outcome.mean" = a.outcome.mean$outcome,
                             "outcome.sum" = a.outcome.sum$outcome,
                             "resp.time.median" = a.resp.time$resp.time,
                             "high.var.chosen.mean" = a.high.var.chosen$high.var.chosen,
                             "highEV.mean" = a.highEV$highEV)
df.participant$workerid <- as.character(df.participant$workerid)
df.participant$condition <- NA

goalReachedPart <- NULL

# loop through participants
for (kk in 1:nrow(df.participant)){
  
  # save condition
  df.participant$condition[kk] <- df.long$condition[df.long$workerid == df.participant$workerid[kk]][1]
  
  # save how often (proportion) a participant in conditions 3 and 4 reached the goal
  if (df.participant$condition[kk] %in% 3:4){
   goalReachedPart <- c(goalReachedPart, mean(df.game$goalReached[df.game$workerid == df.participant$workerid[kk] & df.game$game > 1]))
   
  } else{
    goalReachedPart <- c(goalReachedPart, NA)
    
  }
}

df.participant$goalReached.mean.NP <- goalReachedPart

# read in a survey dataframe as reference
#survey.df <- readRDS(list.surveys[1])
survey.df <- read.table(list.surveys[1], header = T, sep = "\t", as.is = T)

# loop through survey dataframes
for (jj in 2:length(list.surveys)){
  
  # read in files and add to prepared dataframe
  #temp.surv <- readRDS(list.surveys[jj])
  temp.surv <- read.table(list.surveys[jj], header = T, sep = "\t", as.is = T)
  survey.df <- rbind(survey.df, temp.surv)
  
}

# get rid of condition column, it already exists in df.participant
survey.df <- survey.df[, !(names(survey.df) == "condition")]

survey.df$workerid <- as.character(survey.df$workerid)

# sanity check if ids are always the same
mean(df.participant$workerid == survey.df$workerid) == 1

# merge dataframes
df.part <- merge(df.participant, survey.df, by = "workerid")

saveRDS(df.part, "data/dataParticipantLevel.rds")