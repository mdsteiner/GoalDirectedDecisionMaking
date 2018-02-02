rm(list = ls())
gc()
# get modeling results
library(tidyverse)
library(knitr)


load("data/Study1Data/useData/ModelFitOptim01.RData")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")

#### Only needed for model_comparison0X.RData files
# 
# subj_fits <- subj_fits %>%
#   mutate(g2 = case_when(model != "Random" ~ g2 - 2 * log(250),
#                         TRUE ~ g2))
# # Get the best model for each subject
# model_best <- subj_fits %>%
#   group_by(id) %>%
#   summarise(
#     N_models = sum(g2 == min(g2)),       # Number of best fitting models (hopefully 1)
#     model_best = model[g2 == min(g2)][1],
#     model_best_g2 = g2[g2 == min(g2)][1],
#     model_best_Imp = pars_Imp_mle[g2 == min(g2)][1],
#     model_best_Choice = pars_Choice_mle[g2 == min(g2)][1]
#   )
# 
# # Combine actual participant conditions with best model
# 
# model_best <- dat %>%
#   group_by(id) %>%
#   summarise(
#     goal.condition = goal.condition[1],
#     condition = condition[1]
#   ) %>% 
#   ungroup() %>%
#   left_join(model_best)   # Add modelling results
# 


#### Reclassify people classified as RLGoal (because these are so few) to have clearer
####   Results for the other models

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

# Table of results
var_cond <- unique(model_best$variance.condition)


for (i in var_cond){
  print(i)
  print(
    kable(round(
      prop.table(table(model_best$model_best[model_best$variance.condition == i],
                     model_best$goal.condition[model_best$variance.condition == i]), 2),
      3))
  )
}


mean(model_best$model_best_Imp[model_best$model_best == "SampEx_Int_Goal"])


### Summary Statistics for the different models

pars_summary <- model_best %>%
  group_by(model_best, goal.condition) %>%
  summarise(bic_mean = mean(model_best_bic),
            bic_sd = sd(model_best_bic),
            par_Imp_mean = mean(model_best_Imp),
            par_Imp_sd = sd(model_best_Imp),
            par_Choice_mean = mean(model_best_Choice),
            par_Choice_sd = sd(model_best_Choice),
            N = n())

kable(pars_summary)

# separated for environments
pars_summary_envs <- model_best %>%
  group_by(model_best, variance.condition, goal.condition) %>%
  summarise(bic_mean = mean(model_best_bic),
            bic_sd = sd(model_best_bic),
            par_Imp_mean = mean(model_best_Imp),
            par_Imp_sd = sd(model_best_Imp),
            par_Choice_mean = mean(model_best_Choice),
            par_Choice_sd = sd(model_best_Choice),
            N = n())

kable(pars_summary_envs)
