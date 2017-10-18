# issue #3 
# Contrast 2 simple heuristic choice models.
#
# Always select high sample mean option
# Select option favored by RSF
#
# For each participant at each trial, calculate the prediction from each model 
# and compare to the participant's actual choice.
#
#    In what percent of trials do models 1 and 2 differ in their predictions?
#    For how many participants does model 2 outperform model 1?
#    Does model 2 outperform model 1 when people are closer to the goal?

library(yarrr)
l.cols <- piratepal(palette = 'basel', trans = .6)
l.cols.m <- piratepal(palette = 'basel', trans = .1)

df.trial <- readRDS("data/dataTrialLevelPGetthere.rds")

sel.RSF <- NULL
sel.high.samp.mean <- NULL

# first create vectors with predictions of options to choose from
for (ii in 1:nrow(df.trial)){
  
  RSF.i <- which(c(df.trial$p.getthere.1.subj[ii], df.trial$p.getthere.2.subj[ii],
                   df.trial$p.getthere.3.subj[ii]) == max(c(df.trial$p.getthere.1.subj[ii],
                                                            df.trial$p.getthere.2.subj[ii],
                                                            df.trial$p.getthere.3.subj[ii])))
  
  samp.mean.i <- which(c(df.trial$subj.mean.1[ii], df.trial$subj.mean.2[ii],
                         df.trial$subj.mean.3[ii]) == max(c(df.trial$subj.mean.1[ii],
                                                            df.trial$subj.mean.2[ii],
                                                            df.trial$subj.mean.3[ii])))
  
  sel.RSF <- c(sel.RSF, sample(ifelse(length(RSF.i) > 0, RSF.i, 0), 1))
  
  sel.high.samp.mean<- c(sel.high.samp.mean, sample(ifelse(length(samp.mean.i) > 0, samp.mean.i, 0), 1))
}

# save the predictions from the three theories
df.trial$sel.RSF <- sel.RSF
df.trial$sel.high.samp.mean <- sel.high.samp.mean
df.trial$sel.high.EV <- 1

# in what percent of trials do models 1 and 2 (and high EV,i.e. model 3) differ
mean(df.trial$sel.RSF != df.trial$sel.high.samp.mean)
mean(df.trial$sel.RSF != df.trial$sel.high.EV)
mean(df.trial$sel.high.EV != df.trial$sel.high.samp.mean)

# ----------------------------
# For how many participants does model 2 outperform model 1?
# ----------------------------

# create variable with proportion of correct predictions
RSF.df <- aggregate(sel.RSF == selection ~ workerid + condition, FUN = mean, data = df.trial)
high.samp.mean.df <- aggregate(sel.high.samp.mean == selection ~ workerid + condition,
                               FUN = mean, data = df.trial)
sel.high.EV.df <- aggregate(sel.high.EV == selection ~ workerid + condition,
                            FUN = mean, data = df.trial)

# save all in one dataframe
names(RSF.df)[3] <- "RSF.acc"
RSF.df$high.samp.mean.acc <- high.samp.mean.df$`sel.high.samp.mean == selection`
RSF.df$high.EV.acc <- sel.high.EV.df$`sel.high.EV == selection`

# check the proportion of RSF outperforming the select high sample mean option rule
with(RSF.df, mean(RSF.acc > high.samp.mean.acc))
aggregate(RSF.acc > high.samp.mean.acc ~ condition, FUN = mean, data = RSF.df)

# check the proportion of RSF outperforming the select high EV option rule
with(RSF.df, mean(RSF.acc > high.EV.acc))
aggregate(RSF.acc > high.EV.acc ~ condition, FUN = mean, data = RSF.df)

# Does model 2 (RSF) outperform model 1 (high sample mean) when people are closer to the goal?

# create variable with proportion of correct predictions
RSF.df.g <- aggregate(sel.RSF == selection ~ workerid + condition, FUN = mean,
                      data = subset(df.trial, trial >= 40))
high.samp.mean.df <- aggregate(sel.high.samp.mean == selection ~ workerid + condition,
                               FUN = mean, data = subset(df.trial, trial >= 40))
sel.high.EV.df <- aggregate(sel.high.EV == selection ~ workerid + condition,
                            FUN = mean, data = subset(df.trial, trial >= 40))
# save all in one dataframe
names(RSF.df.g)[3] <- "RSF.acc"
RSF.df.g$high.samp.mean.acc <- high.samp.mean.df$`sel.high.samp.mean == selection`
RSF.df.g$high.EV.acc <- sel.high.EV.df$`sel.high.EV == selection`

# check the proportion of RSF outperforming the select high sample mean option rule
with(RSF.df.g, mean(RSF.acc > high.samp.mean.acc))
aggregate(RSF.acc > high.samp.mean.acc ~ condition, FUN = mean, data = RSF.df.g)

# check the proportion of RSF outperforming the select high EV option rule
with(RSF.df.g, mean(RSF.acc > high.EV.acc))
aggregate(RSF.acc > high.EV.acc ~ condition, FUN = mean, data = RSF.df.g)

# let's use a sliding window approach

# define window size and prepare vectors
w.size <- 6
RSF.p <- NULL
samp.mean.p <- NULL
EV.p <- NULL
for (jj in 1:nrow(df.trial)){
  
  # for the first w.size trials don't compute anything
  if(df.trial$trial[jj] < w.size + 1){
    RSF.p <- c(RSF.p, NA)
    samp.mean.p <- c(samp.mean.p, NA)
    EV.p <- c(EV.p, NA)
    
  } else {
    
    # for the other trials compute the proportion of correct predictions
    RSF.p <- c(RSF.p, mean(df.trial$sel.RSF[(jj - w.size): jj] == df.trial$selection[(jj - w.size): jj]))
    samp.mean.p <- c(samp.mean.p, mean(df.trial$sel.high.samp.mean[(jj - w.size): jj] ==
                                         df.trial$selection[(jj - w.size): jj]))
    EV.p <- c(EV.p, mean(df.trial$sel.high.EV[(jj - w.size): jj] ==
                           df.trial$selection[(jj - w.size): jj]))
  }
}

df.trial$RSF.p <- RSF.p
df.trial$samp.mean.p <- samp.mean.p
df.trial$EV.p <- EV.p


# sliding window plot for RSF
windows(height = 22, width = 33)
par(mfrow = c(2,2))
for (cc in 1:4){
  plot(1, type = "n", ylab = "correct pred RSF", xlab = "Trial", ylim = c(0, 1), xlim = c(w.size, 50),
       xaxs = "i", yaxs = "i", main = paste("RSF Condition", cc))
  n.df<- aggregate(RSF.p ~ workerid + trial, FUN = mean, data = subset(df.trial, condition == cc))
  mean(df.trial$RSF.p[df.trial$workerid == 1 & df.trial == 7])
  for (part in unique(n.df$workerid)){
    lines(7:50, n.df$RSF.p[n.df$workerid == part], col = l.cols[cc])
  }
  
  n.df<- aggregate(RSF.p ~ trial, FUN = mean, data = n.df)
  lines(7:50, n.df$RSF.p, col = l.cols.m[cc], lwd =2)

}


# sliding window plot for high sample mean
windows(height = 22, width = 33)
par(mfrow = c(2,2))
for (cc in 1:4){
  plot(1, type = "n", ylab = "correct pred high sample mean", xlab = "Trial", ylim = c(0, 1), xlim = c(w.size, 50), xaxs = "i", yaxs = "i", main = paste("High samp mean Condition", cc))
  n.df<- aggregate(samp.mean.p ~ workerid + trial, FUN = mean, data = subset(df.trial, condition == cc))
  mean(df.trial$samp.mean.p[df.trial$workerid == 1 & df.trial == 7])
  for (part in unique(n.df$workerid)){
    lines(7:50, n.df$samp.mean.p[n.df$workerid == part], col = l.cols[cc])
  }
  
  n.df<- aggregate(samp.mean.p ~ trial, FUN = mean, data = n.df)
  lines(7:50, n.df$samp.mean.p, col = l.cols.m[cc], lwd =2)
  
}


# sliding window plot for high EV chosen
windows(height = 22, width = 33)
par(mfrow = c(2,2))
for (cc in 1:4){
  plot(1, type = "n", ylab = "correct pred high sample mean", xlab = "Trial", ylim = c(0, 1), xlim = c(w.size, 50), xaxs = "i", yaxs = "i", main = paste("High EV Condition", cc))
  n.df<- aggregate(EV.p ~ workerid + trial, FUN = mean, data = subset(df.trial, condition == cc))
  mean(df.trial$EV.p[df.trial$workerid == 1 & df.trial == 7])
  for (part in unique(n.df$workerid)){
    lines(7:50, n.df$EV.p[n.df$workerid == part], col = l.cols[cc])
  }
  
  n.df<- aggregate(EV.p ~ trial, FUN = mean, data = n.df)
  lines(7:50, n.df$EV.p, col = l.cols.m[cc], lwd =2)
  
}


# plot the curves
windows(height = 22, width = 33)
par(mfrow = c(2,2))
for (cc in 1:4){
  plot(1, type = "n", ylab = "proportion of correct prediction", xlab = "Trial", ylim = c(0, 1), xlim = c(7, 50), xaxs = "i", yaxs = "i", main = paste("Condition", cc))
  
  n.df<- aggregate(RSF.p ~ workerid + trial, FUN = mean, data = subset(df.trial, condition == cc))
  n.df<- aggregate(RSF.p ~ trial, FUN = mean, data = n.df)
  lines(7:50, n.df$RSF.p, col = l.cols.m[1], lwd =2)
  
  n.df<- aggregate(samp.mean.p ~ workerid + trial, FUN = mean, data = subset(df.trial, condition == cc))
  n.df<- aggregate(samp.mean.p ~ trial, FUN = mean, data = n.df)
  lines(7:50, n.df$samp.mean.p, col = l.cols.m[2], lwd =2)
  
  n.df<- aggregate(EV.p ~ workerid + trial, FUN = mean, data = subset(df.trial, condition == cc))
  n.df<- aggregate(EV.p ~ trial, FUN = mean, data = n.df)
  lines(7:50, n.df$EV.p, col = l.cols.m[3], lwd =2)
  legend("topleft", c("RSF", "EV max", "high samp mean"), lty = 1, lwd = 2,col = l.cols.m[1:3])
  
  
}
