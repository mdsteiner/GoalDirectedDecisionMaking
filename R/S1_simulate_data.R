rm(list = ls())
gc()

# ------------
# DATA SIMULATION
#
# Study (working title): Need based decision making in a reinforcement learning task.
# 
# Authors:
#   - Markus D. Steiner (markus.d.steiner@gmail.com)
#   - Nathaniel D. Phillips (Nathaniel.D.Phillips.is@gmail.com)
# 
# This script simulates data that is organized in the same way our real data will be. We used the simulated
# data to then write the data preparation script (S1_dataPrep01_PrepareData.R) and the analysis script
# (S1_01_mainAnalysis.R). For each simulated participant, at least four files are created: two or more id files
# (sometimes participants restart the experiment by refreshing the browser window or exhibit a crash, then more than
# two files are created) and two data files (one for the game data and one for the survey file).
# 
# ------------





# ----------------
# create simulation dataframes
# ----------------

### load librarys
if (!require(digest)) install.packages("digest"); library(digest)

### Read learning functions
source("r/learning_functions.R")

### Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

### Simulation parameters

# number of participants to simulate
n.dfs <- 450

n.trials <- 25
n.games <- 10
n.trials.practice <- 20

# mean and sd of the payout dist for the no goal conditions
mean.payout.dist <- 850
sd.payout.dist <- 100

# set the mean and sd for the bonus distribution
mean.bonus <- .8
sd.bonus <- .15



for (kk in 1:n.dfs){
  
  # create a worker id
  workerid.i <- paste(sample(c(letters, LETTERS, 1:9), size = 8, replace = T), collapse = '')
  
  ### Write id data 
  
  # sometimes crashes occur and thus more than two id dataframes are created, this is then used as
  # a control if a participant exhibited a crash or pressed the refresh button
  
  
  times <- sample(c(1, 2), size = 2, replace = T, prob = c(.95, .05))
  
  for (ti in 1:times[1]){
    
    # create id dataframes in the id folder. There should be exaclty two for each participant
    onlyID <- data.frame("workerID" = workerid.i)
    
    # for the game file
    IDDatafileName <- paste0(workerid.i, as.integer(Sys.time()), digest::digest(onlyID), "_g.csv")
    IDDatafilePath <- file.path("data/SimulationData/ids", IDDatafileName)
    write.csv(onlyID, IDDatafilePath, row.names = FALSE, quote = TRUE)
    
    Sys.sleep(1)
  }
  
  for (ti in 1:times[2]){
    
    # create id dataframes in the id folder. There should be exaclty two for each participant
    onlyID <- data.frame("workerID" = workerid.i)
    
    # for the survey file
    IDDatafileName <- paste0(workerid.i, as.integer(Sys.time()), digest::digest(onlyID), "_s.csv")
    IDDatafilePath <- file.path("data/SimulationData/ids", IDDatafileName)
    write.csv(onlyID, IDDatafilePath, row.names = FALSE, quote = TRUE)
    
    Sys.sleep(1)
  }
  
  ### Create game data
  
  # determine condition
  condition <- sample(1:6, 1)
  
  # determine goal and strategy
  if (condition %in% 1:3){
    
    # no goal conditions
    goal <- 0
    
    # determine strategy this participant uses
    strategy.i <- sample(c("ev", "rsf", "random"), size = 1, prob = c(1, 0, 0))
  } else {
    
    # goal conditions
    goal <- 100
    
    # determine strategy this participant uses
    strategy.i <- sample(c("ev", "rsf", "random"), size = 1, prob = c(0, 1, 0))
  }
  
  ## determine environments
  
  # practice game
  m.practice <- c(3, 3)
  sd.practice <- c(2, 2)
  
  if (condition %in% c(1, 4)){
    
    # means
    m.game <- c(4, 4)
    
    # standard deviations
    sd.game <- c(2.5, 11)
  } else if (condition %in% c(2, 5)) {
    
    # means
    m.game <- c(4, 2.5)
    
    # standard deviations
    sd.game <- c(2.5, 11)
    
  } else {
    # means
    m.game <- c(2.5, 4)
    
    # standard deviations
    sd.game <- c(2.5, 11)
  }
  

  
  # determine exploration parameter
  epsilon <- rnorm(1, mean = .2, sd = .1)
  if (epsilon >= 1){epsilon <- .99}
  if (epsilon <= 0){epsilon <- .01}
  
  # determine learning parameter
  alpha <- rnorm(1, mean = .3, sd = .1)
  if (alpha >= 1){alpha <- .99}
  if (alpha <= 0){alpha <- .01}

  # create option order variable. This stores the position (left to right on screen) of the options 1 and 2
  option.order.i <- sample(1:2)
  option.order.string <- paste(option.order.i, collapse = ";")
  
  
  # simulate practice game
  sim.i <- rl.sim.fun(n.trials = n.trials.practice,     # Trials in game
                      option.mean = c(m.practice[option.order.i[1]],
                                      m.practice[option.order.i[2]]),   # Mean of each option
                      option.sd = c(sd.practice[option.order.i[1]],
                                    sd.practice[option.order.i[2]]),   # SD of each option
                      prior.exp.start = rep(0, length(m.practice)), 
                      prior.sd.start = 1,
                      goal = goal,
                      epsilon = epsilon,                     # p(explore | selection.strat = "egreedy")
                      theta = 1, 
                      alpha = alpha,
                      plot = FALSE, 
                      strategy = strategy.i, 
                      ylim = c(0, 100),
                      selection.strat = "egreedy",
                      int.values = TRUE)
  
  # save values
  trial <- 1:n.trials.practice
  selection <- sim.i$selection
  outcome <- sim.i$outcome
  outcome.cum <- sim.i$outcome.cum
  option.order <- rep(option.order.string, n.trials.practice)
  time <- rnorm(n.trials.practice, mean = .8, sd = .2) + runif(n.trials.practice, min = -.25, max = .25)
  
  # now do the same for the 10 games
  
  for (ga in 1:n.games){
    
    ## get some variation in parameters
    
    # determine exploration parameter
    epsilon <- epsilon + rnorm(1, mean = 0, sd = .03)
    if (epsilon >= 1){epsilon <- .99}
    if (epsilon <= 0){epsilon <- .01}
    
    # determine learning parameter
    alpha <- alpha + rnorm(1, mean = 0, sd = .03)
    if (alpha >= 1){alpha <- .99}
    if (alpha <= 0){alpha <- .01}
    
    # create option order variable. This stores the position (left to right on screen) of the options 1 and 2
    option.order.i <- sample(1:2)
    option.order.string <- paste(option.order.i, collapse = ";")
    
    
    # simulate practice game
    sim.i <- rl.sim.fun(n.trials = n.trials,     # Trials in game
                        option.mean = c(m.game[option.order.i[1]],
                                        m.game[option.order.i[2]]),   # Mean of each option
                        option.sd = c(sd.game[option.order.i[1]],
                                      sd.game[option.order.i[2]]),   # SD of each option
                        prior.exp.start = rep(0, length(m.game)), 
                        prior.sd.start = 1,
                        goal = goal,
                        epsilon = epsilon,            # p(explore | selection.strat = "egreedy")
                        theta = 1, 
                        alpha = alpha,
                        plot = FALSE, 
                        strategy = strategy.i, 
                        ylim = c(0, 100),
                        selection.strat = "egreedy",
                        int.values = TRUE)
    
    # save values
    trial <- c(trial, 1:n.trials)
    selection <- c(selection, sim.i$selection)
    outcome <- c(outcome, sim.i$outcome)
    outcome.cum <- c(outcome.cum, sim.i$outcome.cum)
    option.order <- c(option.order, rep(option.order.string, n.trials))
    time <- c(time, 
              rnorm(n.trials, mean = .8, sd = .2) + runif(n.trials, min = -.25, max = .25))
  }

  # create game vector
  game <- c(rep(1, n.trials.practice), rep(c(2:(n.games + 1)), each = n.trials))
  
  # set minimum time value to 0.5 (can't be lower because of the way the exp is programed)
  time[time < .5] <- .5
  
  # create game dataframe
  GameData.i <- data.frame("trial" = trial,
                           "time" = time,
                           "selection" = selection, 
                           "outcome" = outcome,
                           "game" = game,
                           "points.cum" = outcome.cum,
                           "option.order" = option.order,
                           "workerid" = workerid.i,
                           "goal" = goal,
                           "condition" = condition)
  
  # get number of goals reached (in the no goal conditions this is NA) adn add to df
  nGoalsReached <- ifelse(condition %in% 4:6, sum(outcome.cum[trial == n.trials] > goal), NA)
  GameData.i$n.goals.reached <- nGoalsReached
  
  # create answer to check questions
  checkFails <- sample(0:4, 1, prob = c(.8, .125, .05, .015, .01))
  GameData.i$checkFails <- checkFails
  
  # determine the payout
  if (condition %in% 4:6){
    
    # in the goal conditions they receive .2$ per goal reached
    payout <- nGoalsReached * .2
    
  } else {
    
    # in the no goal conditions they receive a bonus according to their toalt points earned
    totalPoints <- sum(outcome.cum[trial == n.trials])
    payout <- round(((totalPoints - mean.payout.dist) / sd.payout.dist) * sd.bonus + mean.bonus, 2)
    if (payout < 0) {payout <- 0}
    
  }
  
  # add payout to df
  GameData.i$payout <- payout
  
  # create answer to question about which option had the high ev
  
  if (condition %in% c(1, 4)){
    
    which.high.ev <- sample(c(1, 2, 3), 1, prob = c(.2, .2, .6))
    
  } else if (condition %in% c(2, 5)){
    
    which.high.ev <- sample(c(1, 2, 3), 1, prob = c(.2, .6, .2))
    
  } else {
    
    which.high.ev <- sample(c(1, 2, 3), 1, prob = c(.6, .2, .2))
    
    }

  # add answer to dataframe
  GameData.i$which.high.ev <- which.high.ev
  
  ### save the game file
  IDDatafileName <- paste0(workerid.i, as.integer(Sys.time()), digest::digest(GameData.i), "_g.csv")
  IDDatafilePath <- file.path("data/SimulationData/data", IDDatafileName)
  write.csv(GameData.i, IDDatafilePath, row.names = FALSE, quote = TRUE)
  
  
  ### Create survey data
  
  age.i <- round(runif(1, min = 18, max = 70))
  
  sex.i <- sample(1:3, 1, prob = c(.45, .45, .1))
  
  comment.i <- paste(sample(c(letters, LETTERS, 0:9, ".", ",", " "), runif(1, min = 0, max = 40),
                      replace = T), collapse = "")
  
  completion.code <- paste0("EP-", sample(100:999, size = 1),
                            "-", sample(100:999, size = 1),
                            "-", sample(100:999, size = 1))
  
  ps <- matrix(c(.1, .225, .3, .275, .1, .05, .125, .25, .325, .25), byrow = T, ncol = 5)
  gameDifficulty.i <- sample(1:5, 1,
                            prob = ps[ifelse(condition %in% 1:3, 1, 2),])
  
  strategy.i <- paste(sample(c(letters, LETTERS, 0:9, ".", ",", " "), runif(1, min = 0, max = 40),
                       replace = T), collapse = "")
  
  strategyChange.i <- paste(sample(c(letters, LETTERS, 0:9, ".", ",", " "), runif(1, min = 0, max = 40),
                             replace = T), collapse = "")
  
  whichStrategy.i <- sample(1:2, 1)
  
  instructionsClear.i <- sample(1:2, 1, prob = c(.8, .2))
  
  similarTask.i <- sample(1:2, 1)
  
  notUnderstood.i <- paste(sample(c(letters, LETTERS, 0:9, ".", ",", " "), runif(1, min = 0, max = 40),
                            replace = T), collapse = "")
  
  errorOrBugs.i <- sample(1:2, 1, prob = c(.8, .2))
  
  describeError.i <- paste(sample(c(letters, LETTERS, 0:9, ".", ",", " "), runif(1, min = 0, max = 40),
                                  replace = T), collapse = "")
  
  gaveUp.i <- sample(1:2, 1, prob = c(.2, .8))
  
  tookNotes.i <- sample(1:2, 1, prob = c(.2, .8))
  
  usedCalculator.i <- sample(1:2, 1, prob = c(.2, .8))
  
  gotHelp.i <- sample(1:2, 1, prob = c(.2, .8))
  
  BNT.i <- sample(c(25, runif(1, min = 1, max = 100)), 1)
  
  knewBNT.i <- sample(1:2, 1, prob = c(.3, .7))
  
  interesting.i <- sample(1:5, 1)
  
  education.i <- sample(1:6, 1, prob = c(.1, .2, .3, .25, .1, .05))
  
  attentionCheck.i <- sample(1:3, 1, prob = c(.175, .175, .65))
  
  trustData.i <- sample(1:2, 1, prob = c(.95, .05))
  
  foundHIT.i <- sample(1:3, 1, prob = c(.65, .175, .175))
  
  textAttentionCheck.i <- sample(c("I read the instructions", NA, "didn't read!!!"),
                                 1, prob = c(.65, .175, .175))
  
  caredReachGoal.i <- ifelse(condition %in% 1:3, NA, sample(0:5, 1, prob = c(.05, .15, .3, .3, .1, .1)))
  
  # create survey dataframe
  SurveyData.i <- data.frame("workerid" = workerid.i,
                             "age" = age.i,
                             "sex" = sex.i,
                             "comments" = comment.i,
                             "completion.code" = completion.code,
                             "game.difficulty" = gameDifficulty.i,
                             "strategy" = strategy.i,
                             "strategy.change" = strategyChange.i,
                             "which.strategy" = whichStrategy.i,
                             "instructions.clear" = instructionsClear.i,
                             "similar.task" = similarTask.i,
                             "not.understood" = notUnderstood.i,
                             "error.or.bugs" = errorOrBugs.i,
                             "describeError" = describeError.i,
                             "gave.up" = gaveUp.i,
                             "tookNotes" = tookNotes.i,
                             "usedCalculator" = usedCalculator.i,
                             "got.help" = gotHelp.i,
                             "BNT" = BNT.i,
                             "knewBNT" = knewBNT.i,
                             "interesting" = interesting.i,
                             "education" = education.i,
                             "attention.check" = attentionCheck.i,
                             "trust.data" = trustData.i,
                             "foundHIT" = foundHIT.i,
                             "text.attention.check" = textAttentionCheck.i,
                             "condition" = condition,
                             "caredReachGoal" = caredReachGoal.i)
  
  ### save the game file
  IDDatafileName <- paste0(workerid.i, as.integer(Sys.time()), digest::digest(SurveyData.i), "_s.csv")
  IDDatafilePath <- file.path("data/SimulationData/data", IDDatafileName)
  write.csv(SurveyData.i, IDDatafilePath, row.names = FALSE, quote = TRUE)
  
}










