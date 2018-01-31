# Plots for EE-Goals Paper

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(afex)) install.packages("afex"); library(afex)
if (!require(coin)) install.packages("coin"); library(coin)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/ModelSimDat_All.rds")

# use only RL simulated data for NoGoal and SampEx data for Goal condition
df.trial <- df.trial %>%
  filter(model == "RL" | model == "SampEx_Int_Goal") %>%
  mutate(goal.condition = case_when(model == "RL" ~ "NoGoal",
                                    model == "SampEx_Int_Goal" ~ "Goal"))

# aggregate to participant level
df.participant <- df.trial %>%
  group_by(id,  goal.condition, variance_condition, model) %>%
  summarise(
    points.cum = sum(outcome),
    high.var.chosen.rate = mean(high.var.chosen, na.rm = T)
  )

risky.ag <- NULL
risky.ug <- NULL

for (nn in unique(df.trial$id)){
  
  risky.ag <- c(risky.ag,
                mean(df.trial$selection[df.trial$id == nn & df.trial$points.cum >=  100]
                     == 2, na.rm = TRUE))
  risky.ug <- c(risky.ug,
                mean(df.trial$selection[df.trial$id == nn & df.trial$points.cum < 100]
                     == 2, na.rm = TRUE))
}

df.participant$risky.ag <- risky.ag
df.participant$risky.ug <- risky.ug


names(df.participant)[2:3] <- c("Goal Condition", "Variance Condition")
df.participant$`Goal Condition`[df.participant$`Goal Condition` == "NoGoal"] <- "No Goal"

temp.df <- data.frame("State" = rep(c("Below 100 Points", "Above 100 Points"),
                                    each = nrow(df.participant)),
                      "Goal Condition" = rep(df.participant$`Goal Condition`, 2),
                      "Risky" = c(df.participant$risky.ug, df.participant$risky.ag))
names(temp.df)[2] <- "Goal Condition"

pdf("plot/pRiskyAboveUnderGoalNoVarCondSimulation.pdf", width = 12.5, height = 5.5)
par(mar=c(5,8.5,3,1.5), mfrow = c(1, 1))
yarrr::pirateplot(Risky ~ State + `Goal Condition`, data = temp.df,
                  ylab = "p Risky chosen", xlab = "Conditions", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()

