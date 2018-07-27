rm(list = ls())
gc()
# get modeling results
library(tidyverse)
library(knitr)
library(yarrr)


load("data/Study1Data/useData/ParameterRecoveryOptim02.RData")
df_sim <- readRDS("data/Study1Data/useData/ModelSimDat_All.rds")


####   Results for the other models

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
  left_join(model_best)   # Add modelling results

model_best$corr_class <- as.numeric(model_best$model == model_best$model_best)

with(model_best, tapply(model == model_best, model, mean, na.rm = T))

kable(with(model_best, tapply(model == model_best, model, mean, na.rm = T)))

kable(round(prop.table(table(model_best$model, model_best$model_best)/250, 1), 3))

plot(model_best$pars_Choice[model_best$model == "GoalHeur" & model_best$model_best == "Random"])
plot(model_best$pars_Imp[model_best$model == "GoalHeur" & model_best$model_best == "Random"])

plot(model_best$pars_Choice[model_best$model == "ThreshHeur" & model_best$model_best == "Random"])
plot(model_best$pars_Imp[model_best$model == "ThreshHeur" & model_best$model_best == "Random"])

check_ids <- unique(model_best$id[model_best$model == "SampEx_Heur_Goal" & model_best$model_best == "Random"])
check_ids2 <- unique(model_best$id[model_best$model == "SampEx_Heur_Goal" & model_best$model_best == "SampEx_Heur_Goal"])

subj_fits$pars_Imp_mle[subj_fits$id %in% check_ids & subj_fits$model == "SampEx_Heur_Goal"]

par(mfrow = c(2, 1))
hist(subj_fits$pars_Choice_mle[subj_fits$id %in% check_ids & subj_fits$model == "SampEx_Heur_Goal"], xlim = c(0, 3), main = "Choice Pars Wrong Recovery", xlab = "Choice Parameter")
hist(subj_fits$pars_Choice_mle[subj_fits$id %in% check_ids2 & subj_fits$model == "SampEx_Heur_Goal"], xlim = c(0, 3), main = "Choice Pars Correct Recovery", xlab = "Choice Parameter")

hist(subj_fits$pars_Imp_mle[subj_fits$id %in% check_ids & subj_fits$model == "SampEx_Heur_Goal"], xlim = c(0, 15), main = "Imp Pars Wrong Recovery", xlab = "Impression Parameter")
hist(subj_fits$pars_Imp_mle[subj_fits$id %in% check_ids2 & subj_fits$model == "SampEx_Heur_Goal"], xlim = c(0, 15), main = "Imp Pars Correct Recovery", xlab = "Impression Parameter")
