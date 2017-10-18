# ---------------------
# issue #2: Calculate, and plot, the probability of selecting the RSF best option at each trial
# ---------------------


# - Create a plot showing the proportion of participants that select the risk sensitive foraging
#   (RSF) best option on each trial.
# - Compare this to a plot showing the probability of selecting the high EV option
# 
# This could answer Rui's question: Are people more likely to select the RSF option as they 
# get closer to the goal?


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

# aggregate data over trials
RSF.df <- aggregate(sel.RSF == selection ~ trial + condition, FUN = mean, data = df.trial)
EV.df <- aggregate(sel.high.EV == selection ~ trial + condition, FUN = mean, data = df.trial)
high.samp.mean.df <- aggregate(sel.high.samp.mean == selection ~ trial + condition, FUN = mean, data = df.trial)

# plot the curves
windows(height = 22, width = 33)
par(mfrow = c(2,2))
for (cc in 1:4){
  plot(1, type = "n", ylab = "proportion of correct prediction", xlab = "Trial", ylim = c(0, 1), xlim = c(1, 50), xaxs = "i", yaxs = "i", main = paste("Condition", cc))
  lines(1:50, RSF.df$`sel.RSF == selection`[RSF.df$condition == cc], col = l.cols.m[1], lwd =2)
  lines(1:50, EV.df$`sel.high.EV == selection`[EV.df$condition == cc], col = l.cols.m[2], lwd =2)
  lines(1:50, high.samp.mean.df$`sel.high.samp.mean == selection`[high.samp.mean.df$condition == cc], col = l.cols.m[3], lwd =2)
  legend("topleft", c("RSF", "EV max", "high samp mean"), lty = 1, lwd = 2,col = l.cols.m[1:3])
         
  
}

# curves are not really different, are the predictions really so similar?
mean(df.trial$sel.high.EV == df.trial$sel.RSF)
mean(df.trial$sel.high.EV == df.trial$sel.high.samp.mean)
mean(df.trial$sel.high.samp.mean == df.trial$sel.RSF)





