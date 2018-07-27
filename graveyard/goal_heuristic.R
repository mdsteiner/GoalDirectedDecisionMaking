# Goal model heuristic

rm(list = ls())

if(!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if(!require(knitr)) install.packages("knitr"); library(knitr)

df_trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")

df_trial$pred_goal_heur <- NA
df_trial$selprob <- NA

for (ii in seq_len(nrow(df_trial))){
  
  if (df_trial$trial[ii] == 1){
    
    df_trial$pred_goal_heur[ii] <- sample(c(1, 2), 1)
    selprob_i <- c(0.5, 0.5)
    
  } else {
    
    df_trial$pred_goal_heur[ii] <- c(1, 2)[(df_trial$outcome[ii - 1] >=
                                         (100 - df_trial$points.cum[ii - 1]) /
                                         (26 - df_trial$trial[ii]) &
                                         df_trial$selection[ii - 1] == 2) + 1]
    
    if (df_trial$pred_goal_heur[ii] == 1){
      
      selprob_i <- c(.9999, .0001)
      
    } else {
      
      selprob_i <- c(.0001, .9999)
      
    }
    
    
    
  }
  
  df_trial$selprob[ii] <- selprob_i[df_trial$selection[ii]]
  
}


ids <- unique(df_trial$id)

df_trial$bic_heur <- NA

for (jj in seq_along(ids)){
  
  df_trial$bic_heur[df_trial$id == ids[jj]] <-
    -2 * sum(log(df_trial$selprob[df_trial$id == ids[jj] & df_trial$game > 1]))
  
}
  
# now let's compare with the other modeling data

load("data/Study1Data/useData/ModelFitOptim01.RData")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")


subj_fits <- subj_fits %>%
  filter(model != "RLGoal")

# Get the best model for each subject
model_best <- subj_fits %>%
  group_by(id) %>%
  summarise(
    N_models = sum(bic == min(bic)),       # Number of best fitting models (hopefully 1)
    model_best = model[bic == min(bic)][1],
    model_best_bic = bic[bic == min(bic)][1],
    model_best_Imp = pars_Imp_mle[bic == min(bic)][1],
    model_best_Choice = pars_Choice_mle[bic == min(bic)][1]
  )

for (kk in seq_along(ids)){
  if (model_best$model_best_bic[model_best$id == ids[kk]] >
      df_trial$bic_heur[df_trial$id == ids[kk]][1]){
    
    model_best$model_best[model_best$id == ids[kk]] <- "goal_heur"
    model_best$model_best_bic[model_best$id == ids[kk]] <-
      df_trial$bic_heur[df_trial$id == ids[kk]][1]
    model_best$model_best_Imp[model_best$id == ids[kk]] <- NA
    model_best$model_best_Choice[model_best$id == ids[kk]] <- NA
    
  }
}

# Combine actual participant conditions with best model

model_best <- dat %>%
  group_by(id) %>%
  summarise(
    goal.condition = goal.condition[1],
    condition = condition[1]
  ) %>%
  ungroup() %>%
  left_join(model_best)   # Add modelling results


model_best <- model_best %>%
  left_join(df.participant[c("id", "variance.condition")], by = "id")


# Table of results
kable(table(model_best$model_best, model_best$goal.condition))
kable(round(prop.table(table(model_best$model_best, model_best$goal.condition), 2), 3))
