if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
df.game <- readRDS("data/Study1Data/useData/S1_dataGameLevel.rds")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")


ggplot(df.participant, aes(x = high.var.chosen.rate, y = choose.highvar.subj.rate,
                           col = goal.condition)) + geom_point(alpha = 0.3) +
  facet_grid(.~variance.condition) + geom_smooth(method="lm") +
  labs(title="high var chosen vs choose high var", y="Choose High Var (RST)",
       x="High Var Chosen") + theme_bw()

ggplot(df.trial, aes(y = high.var.chosen, x = trial, col = factor(overGoal))) +
  geom_point(alpha = 0.3) + facet_grid(~variance.condition+goal.condition) + geom_smooth()

ggplot(df.trial, aes(y = high.var.chosen, x = game, col = goal.condition)) +
  geom_jitter(data = df.game, aes(x = game, y = high.var.chosen.rate),  alpha = 0.15) +
  facet_grid(~variance.condition) + geom_smooth() + theme_bw()


