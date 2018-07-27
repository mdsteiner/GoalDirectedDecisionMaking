rm(list = ls())
gc()

# Plots for EE-Goals Paper

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

# load dataframes

df_trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
df_participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")
df_participant_sim <- readRDS("data/SimulationData/useData/S1_dataParticipantLevel.rds")
df_simPar <- readRDS("data/Study1Data/useData/ParticipantParsSimDat.rds")


## Create figure panel

### Plot Risky goal, under and above goal ------------------------

df_temp <- subset(df_participant, goal.condition == "Goal")
names(df_temp)[4] <- c("Variance Condition")
temp_df <- data.frame("State" = factor(rep(c("Below", "Above"),
                                    each = nrow(df_temp))),
                      "Environment" = factor(rep(df_temp$`Variance Condition`, 2), levels = c("Low", "Equal", "High")),
                      "Risky" = c(df_temp$risky.ug, df_temp$risky.ag))

pdf("plot/pRiskyAboveUnderGoalOnlyGoal.pdf", width = 12.5, height = 5.5)
par(mar=c(5,8.5,3,1.5), mfrow = c(1, 1))
yarrr::pirateplot(Risky ~ State + Environment, data = temp_df,
                  ylab = "Likelihood Risky", xlab = "Conditions", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3, sortx = "sequential")

dev.off()

### Plot Risky goal vs no goal, under and above goal ------------------------

df_temp <- df_participant
names(df_temp)[4] <- c("Variance Condition")

df_temp$`Variance Condition` <- paste(df_temp$`Variance Condition`, "Expected Return")

temp_df <- data.frame("State" = factor(rep(c("Below Goal", "Above Goal"),
                                    each = nrow(df_temp)),
                                    levels = c("Below Goal", "Above Goal")),
                      "Environment" = factor(rep(df_temp$`Variance Condition`, 2),
                                             levels = c("Low Expected Return",
                                                        "Equal Expected Return",
                                                        "High Expected Return")),
                      "Risky" = c(df_temp$risky.ug, df_temp$risky.ag),
                      "Goal" = rep(df_temp$goal.condition, 2))

names(temp_df)[2] <- "Risky Option"

pdf("plot/pRiskyAboveUnderGoal.pdf", width = 13.5, height = 8)
par(mar=c(4.25,6.5,3,0.4), mfrow = c(2, 1))
yarrr::pirateplot(Risky ~ State + `Risky Option`, data = subset(temp_df, Goal == "Goal"),
                  ylab = "Proportion Risky Choices", xlab = "Conditions", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
mtext("Goal Condition", line = -1.9, cex = 1.3, font = 2, adj = 1, side = 2, las = 1, padj = -9.5)

yarrr::pirateplot(Risky ~ State + `Risky Option`, data = subset(temp_df, Goal != "Goal"),
                  ylab = "Proportion Risky Choices", xlab = "Conditions", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
mtext("No-Goal Condition", line = -4, cex = 1.3, font = 2, adj = 1, side = 2, las = 1, padj = -9.5)

dev.off()
### Plot Risky goal vs no goal, under and above goal for SIMULATION DATA ------------------------

df_temp <- subset(df_participant_sim, goal.condition == "Goal")
names(df_temp)[4] <- c("Variance Condition")
temp_df <- data.frame("State" = rep(c("Below", "Above"),
                                    each = nrow(df_temp)),
                      "Environment" = rep(df_temp$`Variance Condition`, 2),
                      "Risky" = c(df_temp$risky.ug, df_temp$risky.ag))

pdf("plot/pRiskyAboveUnderGoalOnlyGoal_SimDat.pdf", width = 12.5, height = 5.5)
par(mar=c(5,8.5,3,1.5), mfrow = c(1, 1))
yarrr::pirateplot(Risky ~ State + Environment, data = temp_df,
                  ylab = "Likelihood Risky", xlab = "Conditions", main = "Preregistered Simulation Data",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3, sortx = "sequential")

dev.off()

### Plots from data simulated by participant parameters ----------

df_sim_participant <- df_simPar %>%
  mutate(State = case_when(points.cum >= 100 ~ "Above",
                           TRUE ~ "Below"),
         State = factor(State, levels = c("Below", "Above")),
         Environment = factor(variance_condition, levels = c("Low", "Equal", "High"))) %>%
  group_by(id, Environment, State, goal_condition, model) %>%
  summarise(
    Risky = mean(selection == 2)
  )



pdf("plot/pRiskyAboveUnderGoal_SimPartPars.pdf", width = 12.5, height = 10.5)
par(mar=c(5,8.5,3,1.5), mfrow = c(2, 1))
yarrr::pirateplot(Risky ~ State + Environment, data = subset(df_sim_participant,
                                                             goal_condition == "Goal"),
                  ylab = "Likelihood Risky", xlab = "Conditions", main = "Goal Condition",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(Risky ~ State + Environment, data = subset(df_sim_participant,
                                                             goal_condition == "NoGoal"),
                  ylab = "Likelihood Risky", xlab = "Conditions", main = "No Goal Condition",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)


dev.off()



rm(list = ls())
gc()

### Regression of points on p risky

library(tidyverse)
library(lme4)
library(sjstats)
library(yarrr)

df_trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")


# get rid of practice trial and factorize data for use in regression model
df_trial <- df_trial %>%
  filter(game > 1) %>%
  mutate(game = game - 2,
         id.f = as.factor(id),
         goal.condition.f = as.factor(goal.condition),
         variance.condition.f = as.factor(variance.condition),
         overGoal.f = as.factor(overGoal),
         goal.condition.bin = as.factor(case_when(goal.condition == "NoGoal" ~ 0,
                                                  goal.condition == "Goal" ~ 1)),
         goal.dist = (100 - points.cum) / (26 - trial) )


# ----------------------
# Plot data
# ----------------------


# Now make bins of data to get probability values
get_bins <- function(id, goal_cond, var_cond, point_vec, high_var_vec, nbins){
  
  # create bins
  bin_size <- (max(point_vec) - min(point_vec)) / nbins
  
  bin_int <- cumsum(c(min(point_vec, na.rm = TRUE), rep(bin_size, nbins)))
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  high_var_mean <- unlist(lapply(seq_len(nbins),
                                 function(x, point_vec, bin_int, high_var_vec){
                                   mean(high_var_vec[point_vec >= bin_int[x] &
                                                       point_vec < bin_int[x+1]],
                                        na.rm = TRUE)
                                 },
                                 point_vec = point_vec, bin_int = bin_int,
                                 high_var_vec = high_var_vec))
  
  # compute the mean of the values from the bin
  bin_mean <- unlist(lapply(seq_len(nbins),
                            function(x, point_vec, bin_int, high_var_vec){
                              mean(point_vec[point_vec >= bin_int[x] &
                                               point_vec < bin_int[x+1]],
                                   na.rm = TRUE)
                            },
                            point_vec = point_vec, bin_int = bin_int,
                            high_var_vec = high_var_vec))
  
  # get the number of observations that were used in each bin
  numobs <- unlist(lapply(seq_len(nbins),
                          function(x, point_vec, bin_int, high_var_vec){
                            vec <- high_var_vec[point_vec >= bin_int[x] &
                                                  point_vec < bin_int[x+1]]
                            vec <- vec[!is.na(vec)]
                            
                            lvec <- length(vec)
                            
                            lvec
                            
                          },
                          point_vec = point_vec, bin_int = bin_int,
                          high_var_vec = high_var_vec))
  
  # bind to data frame
  df_bin <- tibble("id" = id,
                   "pRisky" = high_var_mean,
                   "mean_bin" = bin_mean,
                   "nObs" = numobs,
                   "goal.condition" = goal_cond,
                   "variance.condition" = var_cond
  )
  
}

# define number of bins
nbins <- 15

# first run to create data frame
id <- unique(df_trial$id)[1]
goal_cond <- df_trial$goal.condition[df_trial$id == id][1]
var_cond <- df_trial$variance.condition[df_trial$id == id][1]
point_vec <- df_trial$points.cum[df_trial$id == id]
high_var_vec <- df_trial$high.var.chosen[df_trial$id == id]

bin_df <- get_bins(id, goal_cond, var_cond, point_vec, high_var_vec, nbins)


# Now go through the rest of the subjects and bind the data frames
for (sub in 2:length(unique(df_trial$id))){
  
  id <- unique(df_trial$id)[sub]
  goal_cond <- df_trial$goal.condition[df_trial$id == id][1]
  var_cond <- df_trial$variance.condition[df_trial$id == id][1]
  point_vec <- df_trial$points.cum[df_trial$id == id]
  high_var_vec <- df_trial$high.var.chosen[df_trial$id == id]
  
  bin_df_temp <- get_bins(id, goal_cond, var_cond, point_vec, high_var_vec, nbins)
  
  bin_df <- rbind(bin_df, bin_df_temp)
  
}

bin_df$variance.condition <- factor(bin_df$variance.condition, levels = c("Low", "Equal", "High"))

pdf("plot/pRiskyByPointsSepEnvs.pdf", width = 12.5, height = 5.5)
# separate for environments
temp_col <- piratepal("basel")

cols <- c("Goal" = transparent(temp_col[[1]], .2),
          "NoGoal" = transparent(temp_col[[2]], .2))


ggplot(bin_df, aes(x = mean_bin, y = pRisky)) + 
  geom_line(data = filter(bin_df, goal.condition == "Goal"),
            aes(group=id), col = temp_col[[1]], lwd = .3, alpha = 0.1) +
  geom_line(data = filter(bin_df, goal.condition == "NoGoal"),
            aes(group=id), col = temp_col[[2]], lwd = .3, alpha = 0.1) +
  geom_smooth(data = filter(bin_df, goal.condition == "Goal"),
              aes(col = "Goal"), se = TRUE, method ="loess", lwd = 1.5) +
  geom_smooth(data = filter(bin_df, goal.condition == "NoGoal"),
              aes(col = "NoGoal"), se = TRUE, method = "loess", lwd = 1.5) +
  scale_colour_manual(name="Goal Conditions",values = cols) +
  geom_vline(xintercept = 100, lty = 2, col = "darkgray", lwd = .9) +
  ylim(0,1) + xlim(-14, 130) +
  ylab("Proportion Risky Choices") + xlab("Total Points") +
  facet_wrap(~ variance.condition) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2),
        axis.title.y = element_text(size = 15, vjust=0.3),
        strip.text.x = element_text(face = "bold"))
dev.off()

# ------------
# Same plots for number of points needed by trial
# ------------


# define number of bins
nbins <- 15

# first run to create data frame
id <- unique(df_trial$id)[1]
goal_cond <- df_trial$goal.condition[df_trial$id == id][1]
var_cond <- df_trial$variance.condition[df_trial$id == id][1]
point_vec <- df_trial$goal.dist[df_trial$id == id]
high_var_vec <- df_trial$high.var.chosen[df_trial$id == id]

bin_df <- get_bins(id, goal_cond, var_cond, point_vec, high_var_vec, nbins)


# Now go through the rest of the subjects and bind the data frames
for (sub in 2:length(unique(df_trial$id))){
  
  id <- unique(df_trial$id)[sub]
  goal_cond <- df_trial$goal.condition[df_trial$id == id][1]
  var_cond <- df_trial$variance.condition[df_trial$id == id][1]
  point_vec <- df_trial$goal.dist[df_trial$id == id]
  high_var_vec <- df_trial$high.var.chosen[df_trial$id == id]
  
  bin_df_temp <- get_bins(id, goal_cond, var_cond, point_vec, high_var_vec, nbins)
  
  bin_df <- rbind(bin_df, bin_df_temp)
  
}



pdf("plot/pRiskyByMeanPointsNeeded.pdf", width = 6.5, height = 5.5)
# separate for environments
temp_col <- piratepal("basel")

cols <- c("Equal" = temp_col[[3]],
          "High" = temp_col[[4]], "Low" = temp_col[[5]])
ggplot(bin_df, aes(x = mean_bin, y = pRisky)) + 
  geom_line(data = filter(bin_df, goal.condition == "Goal" &
                            variance.condition == "Equal"),
            aes(group=id), col = temp_col[[3]], lwd = .3, alpha = 0.1) +
  geom_line(data = filter(bin_df, goal.condition == "Goal" &
                            variance.condition == "High"),
            aes(group=id), col = temp_col[[4]], lwd = .3, alpha = 0.1) +
  geom_line(data = filter(bin_df, goal.condition == "Goal" &
                            variance.condition == "Low"),
            aes(group=id), col = temp_col[[5]], lwd = .3, alpha = 0.1) +
  geom_smooth(data = filter(bin_df, goal.condition == "Goal" &
                              variance.condition == "Equal"),
              aes(x = mean_bin, y = pRisky, col = "Equal"), se = FALSE, method ="loess", lwd = 1.5) +
  geom_smooth(data = filter(bin_df, goal.condition == "Goal" &
                              variance.condition == "High"),
              aes(x = mean_bin, y = pRisky, col = "High"), se = FALSE, method ="loess", lwd = 1.5) +
  geom_smooth(data = filter(bin_df, goal.condition == "Goal" &
                              variance.condition == "Low"),
              aes(x = mean_bin, y = pRisky, col = "Low"), se = FALSE, method ="loess", lwd = 1.5) +
  scale_colour_manual(name="Environments",values=cols) +
  ylim(0,1) + xlim(-15, 15) +
  ylab("Proportion Risky Chosen") + xlab("Average Points Needed Per Trial") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3))
dev.off()




### Create a Panel of the plots --------

lmat <- layout(matrix(c(1, 1, 2, 3, 3, 3), nrow = 2, byrow = TRUE))
layout.show(lmat)


if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

# load dataframes

df_trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
df_participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")


### Plot Risky goal vs no goal, under and above goal ------------------------

df_temp <- subset(df_participant, goal.condition == "Goal")
names(df_temp)[4] <- c("Variance Condition")
temp_df <- data.frame("State" = rep(c("Below", "Above"),
                                    each = nrow(df_temp)),
                      "Environment" = rep(df_temp$`Variance Condition`, 2),
                      "Risky" = c(df_temp$risky.ug, df_temp$risky.ag))

#pdf("plot/pRiskyAboveUnderGoalOnlyGoal.pdf", width = 12.5, height = 5.5)

par(mar=c(5,8.5,3,1.5))
yarrr::pirateplot(Risky ~ State + Environment, data = temp_df,
                  ylab = "p Risky chosen", xlab = "Conditions", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

df_trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")


# get rid of practice trial and factorize data for use in regression model
df_trial <- df_trial %>%
  filter(game > 1) %>%
  mutate(game = game - 2,
         id.f = as.factor(id),
         goal.condition.f = as.factor(goal.condition),
         variance.condition.f = as.factor(variance.condition),
         overGoal.f = as.factor(overGoal),
         goal.condition.bin = as.factor(case_when(goal.condition == "NoGoal" ~ 0,
                                                  goal.condition == "Goal" ~ 1)),
         goal.dist = (100 - points.cum) / (26 - trial) )


# ----------------------
# Plot data
# ----------------------


# Now make bins of data to get probability values
get_bins <- function(id, goal_cond, var_cond, point_vec, high_var_vec, nbins){
  
  # create bins
  bin_size <- (max(point_vec) - min(point_vec)) / nbins
  
  bin_int <- cumsum(c(min(point_vec, na.rm = TRUE), rep(bin_size, nbins)))
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  high_var_mean <- unlist(lapply(seq_len(nbins),
                                 function(x, point_vec, bin_int, high_var_vec){
                                   mean(high_var_vec[point_vec >= bin_int[x] &
                                                       point_vec < bin_int[x+1]],
                                        na.rm = TRUE)
                                 },
                                 point_vec = point_vec, bin_int = bin_int,
                                 high_var_vec = high_var_vec))
  
  # compute the mean of the values from the bin
  bin_mean <- unlist(lapply(seq_len(nbins),
                            function(x, point_vec, bin_int, high_var_vec){
                              mean(point_vec[point_vec >= bin_int[x] &
                                               point_vec < bin_int[x+1]],
                                   na.rm = TRUE)
                            },
                            point_vec = point_vec, bin_int = bin_int,
                            high_var_vec = high_var_vec))
  
  # get the number of observations that were used in each bin
  numobs <- unlist(lapply(seq_len(nbins),
                          function(x, point_vec, bin_int, high_var_vec){
                            vec <- high_var_vec[point_vec >= bin_int[x] &
                                                  point_vec < bin_int[x+1]]
                            vec <- vec[!is.na(vec)]
                            
                            lvec <- length(vec)
                            
                            lvec
                            
                          },
                          point_vec = point_vec, bin_int = bin_int,
                          high_var_vec = high_var_vec))
  
  # bind to data frame
  df_bin <- tibble("id" = id,
                   "pRisky" = high_var_mean,
                   "mean_bin" = bin_mean,
                   "nObs" = numobs,
                   "goal.condition" = goal_cond,
                   "variance.condition" = var_cond
  )
  
}

# ------------
# plots for number of points needed by trial
# ------------


# define number of bins
nbins <- 15

# first run to create data frame
id <- unique(df_trial$id)[1]
goal_cond <- df_trial$goal.condition[df_trial$id == id][1]
var_cond <- df_trial$variance.condition[df_trial$id == id][1]
point_vec <- df_trial$goal.dist[df_trial$id == id]
high_var_vec <- df_trial$high.var.chosen[df_trial$id == id]

bin_df <- get_bins(id, goal_cond, var_cond, point_vec, high_var_vec, nbins)


# Now go through the rest of the subjects and bind the data frames
for (sub in 2:length(unique(df_trial$id))){
  
  id <- unique(df_trial$id)[sub]
  goal_cond <- df_trial$goal.condition[df_trial$id == id][1]
  var_cond <- df_trial$variance.condition[df_trial$id == id][1]
  point_vec <- df_trial$goal.dist[df_trial$id == id]
  high_var_vec <- df_trial$high.var.chosen[df_trial$id == id]
  
  bin_df_temp <- get_bins(id, goal_cond, var_cond, point_vec, high_var_vec, nbins)
  
  bin_df <- rbind(bin_df, bin_df_temp)
  
}


# separate for environments
temp_col <- piratepal("basel")

cols <- c("Equal" = temp_col[[3]],
          "High" = temp_col[[4]], "Low" = temp_col[[5]])
ggplot(bin_df, aes(x = mean_bin, y = pRisky)) + 
  geom_line(data = filter(bin_df, goal.condition == "Goal" &
                            variance.condition == "Equal"),
            aes(group=id), col = temp_col[[3]], lwd = .3, alpha = 0.1) +
  geom_line(data = filter(bin_df, goal.condition == "Goal" &
                            variance.condition == "High"),
            aes(group=id), col = temp_col[[4]], lwd = .3, alpha = 0.1) +
  geom_line(data = filter(bin_df, goal.condition == "Goal" &
                            variance.condition == "Low"),
            aes(group=id), col = temp_col[[5]], lwd = .3, alpha = 0.1) +
  stat_smooth(data = filter(bin_df, goal.condition == "Goal" &
                              variance.condition == "Equal"),
              aes(col = "Equal"), se = FALSE, method ="loess", lwd = 1.5) +
  stat_smooth(data = filter(bin_df, goal.condition == "Goal" &
                              variance.condition == "High"),
              aes(col = "High"), se = FALSE, method ="loess", lwd = 1.5) +
  stat_smooth(data = filter(bin_df, goal.condition == "Goal" &
                              variance.condition == "Low"),
              aes(col = "Low"), se = FALSE, method ="loess", lwd = 1.5) +
  scale_colour_manual(name="Goal Conditions",values=cols) +
  ylim(0,1) + xlim(-15, 15) +
  ylab("Likelihood Risky") + xlab("Points Needed Per Trial") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3))



# ---------------
# Plots of pRisky by trials
# ---------------

# define number of bins
nbins <- 15

# first run to create data frame
id <- unique(df_trial$id)[1]
goal_cond <- df_trial$goal.condition[df_trial$id == id][1]
var_cond <- df_trial$variance.condition[df_trial$id == id][1]
point_vec <- df_trial$points.cum[df_trial$id == id]
high_var_vec <- df_trial$high.var.chosen[df_trial$id == id]

bin_df <- get_bins(id, goal_cond, var_cond, point_vec, high_var_vec, nbins)


# Now go through the rest of the subjects and bind the data frames
for (sub in 2:length(unique(df_trial$id))){
  
  id <- unique(df_trial$id)[sub]
  goal_cond <- df_trial$goal.condition[df_trial$id == id][1]
  var_cond <- df_trial$variance.condition[df_trial$id == id][1]
  point_vec <- df_trial$points.cum[df_trial$id == id]
  high_var_vec <- df_trial$high.var.chosen[df_trial$id == id]
  
  bin_df_temp <- get_bins(id, goal_cond, var_cond, point_vec, high_var_vec, nbins)
  
  bin_df <- rbind(bin_df, bin_df_temp)
  
}

# separate for environments
temp_col <- piratepal("basel")

cols <- c("Goal" = temp_col[[1]],
          "NoGoal" = temp_col[[2]])

ggplot(bin_df, aes(x = mean_bin, y = pRisky)) + 
  geom_line(data = filter(bin_df, goal.condition == "Goal"),
            aes(group=id), col = temp_col[[1]], lwd = .3, alpha = 0.1) +
  geom_line(data = filter(bin_df, goal.condition == "NoGoal"),
            aes(group=id), col = temp_col[[2]], lwd = .3, alpha = 0.1) +
  stat_smooth(data = filter(bin_df, goal.condition == "Goal"),
              aes(col = "Goal"), se = FALSE, method ="loess", lwd = 1.5) +
  stat_smooth(data = filter(bin_df, goal.condition == "NoGoal"),
              aes(col = "NoGoal"), se = FALSE, method = "loess", lwd = 1.5) +
  scale_colour_manual(name="Goal Conditions",values=cols) +
  ylim(0,1) + xlim(-25, 150) +
  ylab("Likelihood Risky") + xlab("Points") +
  facet_wrap(~ variance.condition) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3))
#dev.off()

