rm(list = ls())
gc()

# ---------------------
# Issue 11: Redo Plots that show switch rates, high variance chosen etc. over trials with cumulative points on x -axis
# ---------------------

# To have a better measure of when participants were closer to the goal, don't use trials as measure but the cumulative
# points. Maybe use bins of 5 points or so to make sure to have enough datapoints to get a meaningful mean value.

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(binr)) install.packages("binr"); library(binr)

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

# bin points.cum
bin.borders <- bins.getvals(bins(df.trial$points.cum, 50, minpts = 4))
df.trial$points.cum.bins <- cut(df.trial$points.cum, attr(bin.borders, "binlo"))
df.trial$points.cum.bins.l <- cut(df.trial$points.cum, attr(bin.borders, "binlo"),
                                  labels = 1:(length(attr(bin.borders, "binlo")) -1))



# aggregate data over bins
RSF.df <- aggregate(sel.RSF == selection ~ points.cum.bins.l + condition, FUN = mean, data = df.trial)
EV.df <- aggregate(sel.high.EV == selection ~ points.cum.bins.l + condition, FUN = mean, data = df.trial)
high.samp.mean.df <- aggregate(sel.high.samp.mean == selection ~ points.cum.bins.l + condition, FUN = mean, data = df.trial)


# plot the curves
windows(height = 22, width = 33)
par(mfrow = c(2,2))
for (cc in 1:4){
  plot(1, type = "n", ylab = "proportion of correct prediction", xlab = "Bin", ylim = c(0, 1), xlim = c(1, 50),
       xaxs = "i", yaxs = "i", main = paste("Condition", cc))
  lines(1:49, RSF.df$`sel.RSF == selection`[RSF.df$condition == cc], col = l.cols.m[1], lwd =2)
  lines(1:49, EV.df$`sel.high.EV == selection`[EV.df$condition == cc], col = l.cols.m[2], lwd =2)
  lines(1:49, high.samp.mean.df$`sel.high.samp.mean == selection`[high.samp.mean.df$condition == cc],
        col = l.cols.m[3], lwd =2)
  abline(v = 48, col = l.cols.m[4], lwd = 2)
  legend("topleft", c("RSF", "EV max", "high samp mean"), lty = 1, lwd = 2,col = l.cols.m[1:3])
  
}


# aggregate data over bins
n.df <- aggregate(high.var.chosen ~ points.cum.bins.l + condition, FUN = mean, data = df.trial)

# plot the curves
windows(height = 22, width = 33)
par(mfrow = c(1,1))
plot(1, type = "n", ylab = "proportion of high var chosen", xlab = "Bin", ylim = c(0, 1), xlim = c(1, 50), xaxs = "i",
     yaxs = "i")
lines(n.df$points.cum.bins.l[n.df$condition == 1], n.df$high.var.chosen[n.df$condition == 1], col = l.cols.m[1],
      lwd =2)
lines(n.df$points.cum.bins.l[n.df$condition == 2], n.df$high.var.chosen[n.df$condition == 2], col = l.cols.m[2],
      lwd =2)
lines(n.df$points.cum.bins.l[n.df$condition == 3], n.df$high.var.chosen[n.df$condition == 3],
      col = l.cols.m[1], lwd =2, lty = 2)
lines(n.df$points.cum.bins.l[n.df$condition == 4], n.df$high.var.chosen[n.df$condition == 4],
      col = l.cols.m[2], lwd =2, lty = 2)
abline(v = 48, col = l.cols.m[3], lwd = 2)
legend("topleft", c("LN", "HN", "LG", "HG"), lty = c(1, 1, 2, 2), lwd = 2,col = c(l.cols.m[1:2], l.cols.m[1:2]))




# aggregate data over bins
n.df <- aggregate(switched ~ points.cum.bins.l + condition, FUN = mean, data = df.trial)

# plot the curves
windows(height = 22, width = 33)
par(mfrow = c(1,1))
plot(1, type = "n", ylab = "proportion of switches", xlab = "Bin", ylim = c(0, 1), xlim = c(1, 50), xaxs = "i",
     yaxs = "i")
lines(n.df$points.cum.bins.l[n.df$condition == 1], n.df$switched[n.df$condition == 1], col = l.cols.m[1], lwd =2)
lines(n.df$points.cum.bins.l[n.df$condition == 2], n.df$switched[n.df$condition == 2], col = l.cols.m[2], lwd =2)
lines(n.df$points.cum.bins.l[n.df$condition == 3], n.df$switched[n.df$condition == 3],
      col = l.cols.m[1], lwd =2, lty = 2)
lines(n.df$points.cum.bins.l[n.df$condition == 4], n.df$switched[n.df$condition == 4],
      col = l.cols.m[2], lwd =2, lty = 2)
abline(v = 48, col = l.cols.m[3], lwd = 2)
legend("topleft", c("LN", "HN", "LG", "HG"), lty = c(1, 1, 2, 2), lwd = 2,col = c(l.cols.m[1:2], l.cols.m[1:2]))



# aggregate data over bins
n.df <- aggregate(resp.time ~ points.cum.bins.l + condition, FUN = median, data = df.trial)

# plot the curves
windows(height = 22, width = 33)
par(mfrow = c(1,1))
plot(1, type = "n", ylab = "proportion of switches", xlab = "Bin", ylim = c(0, 2), xlim = c(1, 50), xaxs = "i",
     yaxs = "i")
lines(n.df$points.cum.bins.l[n.df$condition == 1], n.df$resp.time[n.df$condition == 1], col = l.cols.m[1], lwd =2)
lines(n.df$points.cum.bins.l[n.df$condition == 2], n.df$resp.time[n.df$condition == 2], col = l.cols.m[2], lwd =2)
lines(n.df$points.cum.bins.l[n.df$condition == 3], n.df$resp.time[n.df$condition == 3],
      col = l.cols.m[1], lwd =2, lty = 2)
lines(n.df$points.cum.bins.l[n.df$condition == 4], n.df$resp.time[n.df$condition == 4],
      col = l.cols.m[2], lwd =2, lty = 2)
abline(v = 48, col = l.cols.m[3], lwd = 2)
legend("topleft", c("LN", "HN", "LG", "HG"), lty = c(1, 1, 2, 2), lwd = 2,col = c(l.cols.m[1:2], l.cols.m[1:2]))


