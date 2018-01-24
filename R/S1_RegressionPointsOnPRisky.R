rm(list = ls())
gc()

### Regression of points on p risky

library(tidyverse)
library(lme4)
library(sjstats)

df_trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")


# get rid of practice trial and factorize data for use in regression model
df_trial <- df_trial %>%
  filter(game > 1) %>%
  mutate(game = game - 2,
         id.f = as.factor(id),
         goal.condition.f = as.factor(goal.condition),
         overGoal.f = as.factor(overGoal))

df_trialAgg <- df_trial %>%
  group_by(id, variance.condition, goal.condition, trial) %>%
  summarise(risky_rate = mean(high.var.chosen))

# ----------------------
# Plot data
# ----------------------

# try with binary data and regression lines
ggplot(df_trial, aes(x = points.cum, y = high.var.chosen)) + 
  stat_smooth(data = filter(df_trial, goal.condition == "Goal"),
              aes(group=id), method = "loess", formula = y ~ x, se = FALSE,
              col = "blue", lwd = .3, alpha = 0.01) +
  stat_smooth(data = filter(df_trial, goal.condition == "NoGoal"),
              aes(group=id), method = "loess", formula = y ~ x, se = FALSE,
              col = "red", lwd = .3, alpha = 0.01) +
  stat_smooth(data = filter(df_trial, goal.condition == "Goal"),
              aes(),method ="loess",formula = y ~ x, se = FALSE, lwd = 2,
              col = "black") +
  stat_smooth(data = filter(df_trial, goal.condition == "NoGoal"),
              aes(), method = "loess", formula = y ~ x, se = FALSE, lwd = 2,
              col = "grey") +
  ylim(0,1)


# Create plot of pRisky over Trials
cols <- c("Goal" = "red", "NoGoal" = "blue")
ggplot(df_trialAgg, aes(x = trial, y = risky_rate)) + 
  geom_line(data = filter(df_trialAgg, goal.condition == "Goal"),
              aes(group=id), col = "blue", lwd = .3, alpha = 0.1) +
  geom_line(data = filter(df_trialAgg, goal.condition == "NoGoal"),
              aes(group=id), col = "red", lwd = .3, alpha = 0.1) +
  stat_smooth(data = filter(df_trialAgg, goal.condition == "Goal"),
              aes(col = "Goal"),method ="loess", lwd = 1.5) +
  stat_smooth(data = filter(df_trialAgg, goal.condition == "NoGoal"),
              aes(col = "NoGoal"), method = "loess",  lwd = 1.5) +
  scale_colour_manual(name="Goal Conditions",values=cols) +
  ylim(0,1) +
  ylab("Likelihood Risky") + xlab("Trials") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3)) +
  facet_wrap(~ variance.condition)



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
nbins <- 10

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

# plot the bin- and mean lines over all variance conditions
cols <- c("Goal" = "red", "NoGoal" = "blue")
ggplot(bin_df, aes(x = mean_bin, y = pRisky)) + 
  geom_line(data = filter(bin_df, goal.condition == "Goal"),
            aes(group=id), col = "blue", lwd = .3, alpha = 0.1) +
  geom_line(data = filter(bin_df, goal.condition == "NoGoal"),
            aes(group=id), col = "red", lwd = .3, alpha = 0.1) +
  stat_smooth(data = filter(bin_df, goal.condition == "Goal"),
              aes(col = "Goal"), method ="loess", lwd = 1.5) +
  stat_smooth(data = filter(bin_df, goal.condition == "NoGoal"),
              aes(col = "NoGoal"), method = "loess", lwd = 1.5) +
  scale_colour_manual(name="Goal Conditions",values=cols) +
  ylim(0,1) +
  ylab("Likelihood Risky") + xlab("Points") +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3))


# separate for environments
cols <- c("Goal" = "red", "NoGoal" = "blue")
ggplot(bin_df, aes(x = mean_bin, y = pRisky)) + 
  geom_line(data = filter(bin_df, goal.condition == "Goal"),
            aes(group=id), col = "blue", lwd = .3, alpha = 0.1) +
  geom_line(data = filter(bin_df, goal.condition == "NoGoal"),
            aes(group=id), col = "red", lwd = .3, alpha = 0.1) +
  stat_smooth(data = filter(bin_df, goal.condition == "Goal"),
              aes(col = "Goal"), method ="loess", lwd = 1.5) +
  stat_smooth(data = filter(bin_df, goal.condition == "NoGoal"),
              aes(col = "NoGoal"), method = "loess", lwd = 1.5) +
  scale_colour_manual(name="Goal Conditions",values=cols) +
  ylim(0,1) +
  ylab("Likelihood Risky") + xlab("Points") +
  facet_wrap(~ variance.condition) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 15, vjust=-.2)) +
  theme(axis.title.y = element_text(size = 15, vjust=0.3))

# ----------------------
# Run Regression Model
# ----------------------

# mixed effects model with random intercepts for subjects and games
model <- glmer(high.var.chosen ~ goal.condition.f * overGoal.f + points.cum +
                 (1|id.f/game), data = df_trial, family = "binomial")

summary(model)
r2(model)
