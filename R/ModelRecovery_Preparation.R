# Simulate choice patterns with different strategies and try and recover them

# Load libraries
library(tidyverse)

# Read learning functions
source("r/SampExHeurModel.R")

subjects <- 100
games <- 10
trials <- 25

dat <- tibble(
  subj = rep(1:subjects, each = trials * games), # 20 people per model
  game = games, # 10 games
  trial = rep(rep(1:trials, games), subjects), # 25 trials per game
  model = rep(c("SampEx_Heur_Goal", "SampEx_Heur_NoGoal", "SampEx_Int_Goal", "RL", "Random"),
            each = subjects / 5 * games * trials),
  goal = rep(c(100, Inf, 100, Inf, Inf), each = subjects / 5 * games * trials),
  rule_Imp = rep(c("SampExHeur", "SampExHeur", "SampExInt", "RL", "none"),
                 each = subjects / 5 * games * trials),
  rule_Choice = rep(c("Softmax", "Softmax", "Softmax", "Softmax", "none"),
                    each = subjects / 5 * games * trials),
  outc_A = round(rnorm(trials * games * subjects, m_A, sd_A )),
  outc_B = round(rnorm(trials * games * subjects, m_B, sd_B ))
  
)

for (i in 1:nrow(dat)){
  
  
  
}



