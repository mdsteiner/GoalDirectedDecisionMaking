rm(list = ls())
gc()
# get modeling results
library(tidyverse)
library(knitr)
library(yarrr)


load("data/Study1Data/useData/ParameterRecoveryOptim01.RData")
df_sim <- readRDS("data/Study1Data/useData/ModelSimDat_All.rds")

subj_fits <- subj_fits %>%
  filter(model != "RLGoal")

dat <- dat %>%
  filter(model != "RLGoal")

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

pdf("plot/ParameterRecovery.pdf", height = 8, width = 8)
par(mfrow =c(2,2))
# Impression Parameters
plot(model_best$pars_Imp[model_best$model != "SampEx_Int_Goal"],
     model_best$model_best_Imp[model_best$model != "SampEx_Int_Goal"],
     pch = 16, col = transparent("black",.95), main = "RL Model", xlab = "True Impression Parameter",
     ylab = "Fitted Impression Parameter", xlim = c(0, 1), ylim = c(0, 1),
     cex.axis = 1.2, cex.lab = 1.3)
plot(model_best$pars_Imp[model_best$model == "SampEx_Int_Goal"],
     model_best$model_best_Imp[model_best$model == "SampEx_Int_Goal"],
     pch = 16, col = transparent("black",.95), main = "SampEx Model", xlab = "True Impression Parameter",
     ylab = "Fitted Impression Parameter", xlim = c(0, 15), ylim = c(0,15),
     cex.axis = 1.2, cex.lab = 1.3)
# Choice Parameters
plot(model_best$pars_Choice[model_best$model != "SampEx_Int_Goal"],
     model_best$model_best_Choice[model_best$model != "SampEx_Int_Goal"],
     pch = 16, col = transparent("black",.95), main = "RL Model", xlab = "True Choice Parameter",
     ylab = "Fitted Choice Parameter", xlim = c(0, 5), ylim = c(0, 5),
     cex.axis = 1.2, cex.lab = 1.3)
plot(model_best$pars_Choice[model_best$model == "SampEx_Int_Goal"],
     model_best$model_best_Choice[model_best$model == "SampEx_Int_Goal"],
     pch = 16, col = transparent("black",.95), main = "SampEx Model", xlab = "True Choice Parameter",
     ylab = "Fitted Choice Parameter", xlim = c(0, 2.5), ylim = c(0,2.5),
     cex.axis = 1.2, cex.lab = 1.3)

dev.off()


### Check whether the SampEx model predicts higher risky rates below the goal ----

# Aggregate data


pdf("plot/pRiskyAboveUnderGoalOnlyGoal_SimSampEx.pdf", width = 12.5, height = 5.5)
par(mar=c(5,8.5,3,1.5), mfrow = c(1, 1))

df_sim_participant <- df_sim %>%
  filter(model == "SampEx_Int_Goal") %>%
  mutate(State = case_when(points.cum >= 100 ~ "Above",
                           TRUE ~ "Below"),
         State = factor(State, levels = c("Below", "Above")),
         Environment = factor(variance_condition, levels = c("Low", "Equal", "High"))) %>%
  group_by(id, Environment, State) %>%
  summarise(
    Risky = mean(selection == 2)
  )
yarrr::pirateplot(Risky ~ State + Environment, data = df_sim_participant,
                  ylab = "Likelihood Risky", xlab = "Conditions", main = "Simulation SampEx Model",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()


df_sim_participant <- df_sim %>%
  filter(model == "RL") %>%
  mutate(State = case_when(points.cum >= 100 ~ "Above",
                           TRUE ~ "Below"),
         State = factor(State, levels = c("Below", "Above")),
         Environment = factor(variance_condition, levels = c("Low", "Equal", "High"))) %>%
  group_by(id, Environment, State) %>%
  summarise(
    Risky = mean(selection == 2)
  )



pdf("plot/pRiskyAboveUnderGoalOnlyGoal_SimRL.pdf", width = 12.5, height = 5.5)
par(mar=c(5,8.5,3,1.5), mfrow = c(1, 1))
yarrr::pirateplot(Risky ~ State + Environment, data = df_sim_participant,
                  ylab = "Likelihood Risky", xlab = "Conditions", main = "Simulation RL Model",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3, sortx = "sequential")


dev.off()


### Check the goal reached rates of the models-------

df_sim_participant <- df_sim %>%
  filter(trial == 25, model != "RLGoal") %>%
  mutate(Environment = variance_condition,
         Model = case_when(model == "NaturalMean" ~ "Natural Mean",
                           model == "Random" ~ "Random",
                           model == "RL" ~ "RL",
                           TRUE ~ "SampEx")) %>%
  group_by(id, Environment, game, Model) %>%
  summarise(
    goal_reached = case_when(points.cum >= 100 ~ 1,
                             TRUE ~ 0)
  ) %>%
  group_by(id, Environment, Model) %>%
  summarise(
    goal_reached_rate = sum(goal_reached) / 10
  ) #%>%
  # group_by(model, Environment)  %>%
  # summarise(
  #   goal_reached_rate = mean(n_goals_reached) / 10
  # )

pdf("plot/likelihoodGoalReached_ModelSim.pdf", width = 12.5, height = 12)
par(mar=c(5,8.5,3,1.5), mfrow = c(3, 1))
yarrr::pirateplot(goal_reached_rate ~ Model, data = subset(df_sim_participant, Environment == "High"),
                  ylab = "Likelihood Reach Goal", xlab = "Model", main = "High Environment",
                  bean.f.col = c("lightgray"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(goal_reached_rate ~ Model, data = subset(df_sim_participant, Environment == "Equal"),
                  ylab = "Likelihood Reach Goal", xlab = "Model", main = "Equal Environment",
                  bean.f.col = c("lightgray"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(goal_reached_rate ~ Model, data = subset(df_sim_participant, Environment == "Low"),
                  ylab = "Likelihood Reach Goal", xlab = "Model", main = "Low Environment",
                  bean.f.col = c("lightgray"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()


pirateplot(goal_reached_rate ~ model, data = subset(df_sim_participant, Environment == "Low"),
           ylim = c(0,1))
pirateplot(goal_reached_rate ~ model, data = subset(df_sim_participant, Environment == "Equal"),
           ylim = c(0,1))

pirateplot(goal_reached_rate ~ model, data = subset(df_sim_participant, Environment == "High"),
           ylim = c(0,1))


