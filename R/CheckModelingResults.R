# get modeling results

load("data/Study1Data/useData/model_comparison01.RData")


subj_fits <- subj_fits %>%
  mutate(g2 = case_when(model == "SampEx_Int_Goal" ~ g2 - log(250),
                        model != "Random" ~ g2 - 2* log(250),
                        TRUE ~ g2))
# Get the best model for each subject
model_best <- subj_fits %>%
  group_by(id) %>%
  summarise(
    N_models = sum(g2 == min(g2)),       # Number of best fitting models (hopefully 1)
    model_best = model[g2 == min(g2)][1],
    model_best_g2 = g2[g2 == min(g2)][1],
    model_best_Imp = pars_Imp_mle[g2 == min(g2)][1],
    model_best_Choice = pars_Choice_mle[g2 == min(g2)][1]
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

# Table of results
table(model_best$model_best, model_best$goal.condition)




mean(model_best$model_best_Imp[model_best$model_best == "SampEx_Heur_Goal"])
mean(model_best$model_best_Imp[model_best$model_best == "SampEx_Heur_NoGoal"])
