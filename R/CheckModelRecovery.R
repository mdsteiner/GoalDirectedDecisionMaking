rm(list = ls())
gc()
# get modeling results
library(tidyverse)
library(knitr)
library(yarrr)


load("data/Study1Data/useData/ParameterRecoveryOptim01.RData")
df_sim <- readRDS("data/Study1Data/useData/ModelSimDat_All.rds")

df_sim_sub <- df_sim %>%
  

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

with(model_best, tapply(model == model_best, model_best, mean, na.rm = T))

kable(with(model_best, tapply(model == model_best, model_best, mean, na.rm = T)))

par(mfrow =c(2,2))
# Impression Parameters
plot(model_best$pars_Imp[model_best$model != "SampEx_Int_Goal"],
     model_best$model_best_Imp[model_best$model != "SampEx_Int_Goal"],
     pch = 16, col = transparent("black",.95), main = "RL Models", xlab = "True Impression Parameter",
     ylab = "Fitted Impression Parameter", xlim = c(0, 1), ylim = c(0, 1))
plot(model_best$pars_Imp[model_best$model == "SampEx_Int_Goal"],
     model_best$model_best_Imp[model_best$model == "SampEx_Int_Goal"],
     pch = 16, col = transparent("black",.95), main = "SampEx Model", xlab = "True Impression Parameter",
     ylab = "Fitted Impression Parameter", xlim = c(0, 15), ylim = c(0,15))
# Choice Parameters
plot(model_best$pars_Choice[model_best$model != "SampEx_Int_Goal"],
     model_best$model_best_Choice[model_best$model != "SampEx_Int_Goal"],
     pch = 16, col = transparent("black",.95), main = "RL Models", xlab = "True Choice Parameter",
     ylab = "Fitted Choice Parameter", xlim = c(0, 5), ylim = c(0, 5))
plot(model_best$pars_Choice[model_best$model == "SampEx_Int_Goal"],
     model_best$model_best_Choice[model_best$model == "SampEx_Int_Goal"],
     pch = 16, col = transparent("black",.95), main = "SampEx Model", xlab = "True Choice Parameter",
     ylab = "Fitted Choice Parameter", xlim = c(0, 2.5), ylim = c(0,2.5))
