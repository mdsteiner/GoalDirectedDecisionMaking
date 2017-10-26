# Plots for EE-Goals Paper

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(afex)) install.packages("afex"); library(afex)
if (!require(coin)) install.packages("coin"); library(coin)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial <- readRDS("data/Study1Data/useData/S1_dataTrialLevel.rds")
df.trial.eu <- readRDS("data/Study1Data/useData/S1_dataTrialLevelEU.rds")
df.game <- readRDS("data/Study1Data/useData/S1_dataGameLevel.rds")
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")

### Plot Risky goal vs no goal, under and above goal ------------------------

names(df.participant)[3:4] <- c("Goal Condition", "Variance Condition")
df.participant$`Goal Condition`[df.participant$`Goal Condition` == "NoGoal"] <- "No Goal"

pdf("plot/pRiskyAboveUnderGoal.pdf", width = 12, height = 10.5)
par(mar=c(5,9.5,3,1.5), mfrow = c(2, 1))
yarrr::pirateplot(risky.ug ~ `Goal Condition` + `Variance Condition`, data = df.participant,
                  ylab = "p Risky chosen", xlab = "Conditions", main = "Below Goal",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

yarrr::pirateplot(risky.ag ~ `Goal Condition` + `Variance Condition`, data = df.participant,
                  ylab = "p Risky chosen", xlab = "Conditions", main = "Above Goal",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()

### Plot Risky goal vs no goal, under and above goal ------------------------

names(df.participant)[3:4] <- c("Goal Condition", "Variance Condition")
df.participant$`Goal Condition`[df.participant$`Goal Condition` == "NoGoal"] <- "No Goal"

temp.df <- data.frame("State" = rep(c("Below 100 Points", "Above 100 Points"),
                                    each = nrow(df.participant)),
                      "Goal Condition" = rep(df.participant$`Goal Condition`, 2),
                      "Risky" = c(df.participant$risky.ug, df.participant$risky.ag))
names(temp.df)[2] <- "Goal Condition"

pdf("plot/pRiskyAboveUnderGoalNoVarCond.pdf", width = 12.5, height = 5.5)
par(mar=c(5,8.5,3,1.5), mfrow = c(1, 1))
yarrr::pirateplot(Risky ~ State + `Goal Condition`, data = temp.df,
                  ylab = "p Risky chosen", xlab = "Conditions", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()


### Plot the proportion of high variance chosen when rational according to RST ---------------

names(df.trial)[c(17:18, 28)] <- c("Goal Condition", "Variance Condition", "Choose Risky")

df.n <- aggregate(high.var.chosen ~ `Choose Risky` + id + `Goal Condition` + `Variance Condition`,
                  FUN = mean, data = subset(df.trial, game > 1))
df.n$`Goal Condition`[df.n$`Goal Condition` == "NoGoal"] <- "No Goal"

pdf("plot/pRiskyChooseRisky.pdf", width = 12, height = 10.5)

par(mar=c(5,9.5,3,1.5), mfrow = c(2,1))

yarrr::pirateplot(high.var.chosen ~ `Choose Risky` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

yarrr::pirateplot(high.var.chosen ~ `Choose Risky` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "No Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "No Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

dev.off()


# Plot evidence strength -----------------------------

evidence_strength2 <- function(df, strategy, nround, nbatches, withText,
                               returnDf, cex.m, useLines, cex.text, whichType, ...){
  if (strategy == "RST") {
    fav.opt <- round(df$p.getthere.2.subj - df$p.getthere.1.subj, nround)
  } else if (strategy == "EV") {
    fav.opt <- round((df$subj.mean.2 / df$sd.sub2) - (df$subj.mean.1 / df$sd.sub1), nround)
  } else {
    stop("No valid strategy. Strategy must be either EV or RST")
  }
  fav.opt[fav.opt == Inf | fav.opt == -Inf] <- NA
  
  # create batches
  batch.size <- (max(fav.opt, na.rm = TRUE) - min(fav.opt, na.rm = TRUE)) / nbatches
  
  batch.int <- cumsum(c(min(fav.opt, na.rm = TRUE), rep(batch.size, nbatches)))
  
  # compute the probability of choosing the risky option (2), given a certain RSF.diff value
  p.choose.risky <- unlist(lapply(seq_len(nbatches),
                              function(x, df, fav.opt, batch.int){
                                mean(df$selection[fav.opt >= batch.int[x] &
                                                    fav.opt < batch.int[x+1]] == 2,
                                     na.rm = TRUE)
                              },
                              df = df, fav.opt = fav.opt, batch.int = batch.int))
  
  numobs <- unlist(lapply(seq_len(nbatches),
                          function(x, df, fav.opt, batch.int){
                            vec <- df$selection[fav.opt >= batch.int[x] &
                                                  fav.opt < batch.int[x+1]]
                            vec <- vec[!is.na(vec)]
                            
                            lvec <- length(vec)
                            
                            lvec
                            
                          },
                          df = df, fav.opt = fav.opt, batch.int = batch.int))
  
  # scale the size ob points
  total.l <- length(df$selection[!is.na(df$selection)])
  
  numobs.s <- round(numobs / total.l, 2)
  
  
  # plot the results
  if (isTRUE(useLines)){
    lines(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.risky, type = whichType,
          ylab = "p Risky Choice\n", cex = cex.m * exp(numobs.s), ...)
  } else {
    plot(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.risky, type = whichType,
         ylab = "p Risky Choice\n", cex = cex.m * exp(numobs.s), ...)
  }
  
  if (isTRUE(withText)){
    text(x = batch.int[1:(length(batch.int)-1)] + (batch.size / 2), 
         y = p.choose.risky, 
         labels = numobs.s, pos = 3,
         cex = cex.text)
  }
  
  if (isTRUE(returnDf)){
    df.evidence <- data.frame("p.choose.risky" = p.choose.risky,
                              "batch.int" = batch.int[1:(length(batch.int)-1)] + (batch.size / 2),
                              "n.obs" = numobs,
                              "n.obs.s" = numobs.s)
  
    df.evidence
  }
}


# only goal RST
pdf("plot/EvidenceStrengthsRisky.pdf", width = 10, 5.5)#height = 13.125)
# All conditions
par(mfrow = c(1,1), mar =c(5.1, 7.5, 4.1, 2.1))
# Equal 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-1, 1),
                   lty = 2, xlab = "p reach goal risky - p reach goal save",
                   main = "", bty = "l",
                   cex.lab = 1.5, cex.axis = 1.3, las = 1)

# High 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & game > 1), "RST", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("topleft", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)), title = "Environment:",
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 1.5)
dev.off()

### Goal RST and EV, No Goal EV

pdf("plot/EvidenceStrengths.pdf", width = 10, height = 13.125)
# All conditions
par(mfrow = c(3,1), mar =c(5.1, 7.5, 4.1, 2.1))
# Equal 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-1, 1),
                   lty = 2, xlab = "p reach goal risky - p reach goal save",
                   main = "Evidence Strength RST, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & game > 1), "RST", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("topleft", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)
### EV evidence strength

# Equal 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-40, 40),
                   lty = 2, xlab = "d' risky - d' save",
                   main = "Evidence Strength EV, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial, `Goal Condition` == "Goal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("topleft", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)


# Evidence strength EV No Goal


# Equal 
evidence_strength2(subset(df.trial, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-40, 40),
                   lty = 2, xlab = "d' risky - d' save",
                   main = "Evidence Strength EV, No Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial, `Goal Condition` == "NoGoal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("topleft", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

dev.off()



### Prediction accuracy separated for indicated strategy ----------------------

names(df.participant)[c(3:4, 32)] <- c("Goal Condition", "Variance Condition",
                                       "Indicated Strategy")
df.participant$`Goal Condition`[df.participant$`Goal Condition` == "NoGoal"] <- "No Goal"
df.participant$`Indicated Strategy`[df.participant$`Indicated Strategy` == 1] <- "EV"
df.participant$`Indicated Strategy`[df.participant$`Indicated Strategy` == 2] <- "RSF"

pdf("plot/predAccuracy.pdf", width = 12, height = 10.5)
par(mar=c(5,9.5,3,1.5), mfrow = c(2, 1))

yarrr::pirateplot(pred.RSF.acc.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant,
                  main = "RST Model", ylab = "RSF Prediction Accuray", 
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant,
                  main = "EV Model", ylab = "EV Prediction Accuracy",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()

# Plot prediction accuracy ---------------

pdf("plot/predAccuracyModelComparison.pdf")
par(mfrow = c(1, 1), mar = c(5.1, 4.6, 4, 1))
plot(1, xlim = c(0, 1.05), ylim = c(0, 1.05), pch = 16, xlab = "EV", ylab = "RST",
     xaxs = 'i', yaxs = 'i', las = 1, cex.axis = 1.5, cex.lab = 1.75, type = "n")
polygon(c(0, 0, 0.5, 0.5),c(0, 1.05, 1.05, 0), col = rgb(0.62, 0.62, 0.62, .2),
        border = NA)
polygon(c(0, 0, 1.05, 1.05),c(0, 0.5, 0.5, 0), col = rgb(0.62, 0.62, 0.62,.2),
        border = NA)
points(df.participant$pred.EV.acc.rate[df.participant$`Goal Condition` == "Goal"],
       df.participant$pred.RSF.acc.rate[df.participant$`Goal Condition` == "Goal"],
       xlim = c(0, 1), ylim = c(0, 1), pch = 16,
       col = gray(0, .6))
abline(0, 1, lty = 2)
dev.off()


# Plot prediction accuracy separater for conditions ---------------
goals <- rep(c("Goal", "No Goal"), each = 3)
envs <- rep(c("Equal", "High", "Low"), 2)
m <- rbind(
  c(1, rep(2:4, each = 4)),
  c(5, rep(6:8, each = 4)),
  c(5, rep(6:8, each = 4)),
  c(5, rep(6:8, each = 4)),
  c(5, rep(6:8, each = 4)),
  c(9, rep(10:12, each = 4)),
  c(9, rep(10:12, each = 4)),
  c(9, rep(10:12, each = 4)),
  c(9, rep(10:12, each = 4))
  
)
pdf("plot/predAccuracyModelComparisonSepCond.pdf", width = 12, height = 8)

par(mfrow = c(2, 3), mar = c(5.1, 6.5, 3, 1))

for (i in seq_len(6)){
  plot(1, xlim = c(0, 1.05), ylim = c(0, 1.05), pch = 16, xlab = "EV", ylab = "RST",
       xaxs = 'i', yaxs = 'i', las = 1, cex.axis = 1.5, cex.lab = 1.75, type = "n")

  if (i == 1){
    mtext("Goal", font = 2, cex = 1.3, side = 2, line = 5)
    mtext("Equal", font = 2, cex = 1.3, line = 1)
  }
  if (i == 2){
    mtext("High", font = 2, cex = 1.3, line = 1)
  }
  if (i == 3){
    mtext("Low", font = 2, cex = 1.3, line = 1)
  }
  if (i == 4){
    mtext("No Goal", font = 2, cex = 1.3, side = 2, line = 5)
  }
  polygon(c(0, 0, 0.5, 0.5),c(0, 1.05, 1.05, 0), col = rgb(0.62, 0.62, 0.62, .2),
          border = NA)
  polygon(c(0, 0, 1.05, 1.05),c(0, 0.5, 0.5, 0), col = rgb(0.62, 0.62, 0.62,.2),
          border = NA)
  with(df.participant, points(pred.EV.acc.rate[`Goal Condition` == goals[i] &
                                                 `Variance Condition` == envs[i]],
         pred.RSF.acc.rate[`Goal Condition` == goals[i] &
                             `Variance Condition` == envs[i]],
         xlim = c(0, 1), ylim = c(0, 1), pch = 16,
         col = gray(0, .6)))
  abline(0, 1, lty = 2)
  
}
dev.off()
### Prediction Accuracies Barplot ---------

names(df.participant)[3:4] <- c("Goal Condition", "Variance Condition")
df.participant$`Goal Condition`[df.participant$`Goal Condition` == "NoGoal"] <- "No Goal"

temp.df <- data.frame("Prediction Acc" = c(df.participant$pred.EV.acc.rate,
                                           df.participant$pred.RSF.acc.rate),
                      "Model" = rep(c("EV", "RST"), each = nrow(df.participant)),
                      "Goal Condition" = rep(df.participant$`Goal Condition`, 2))
names(temp.df)[c(1, 3)] <- c("Pred Acc", "Goal Condition")

pdf("plot/predictionAccuraciesBarplot.pdf", width = 9, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,8,3,1.5))
pirateplot(`Pred Acc` ~ Model + `Goal Condition`, data = temp.df,
           ylab = "Prediction Accuracy", xlab = "Conditions", main = "",
           bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
           cex.axis = 1.3, cex.names = 1.3)
dev.off()


### Prediction Accuracies Differing predictions ---------

# change this to show trials in people go with rst and ev, rsp.

# aggregate using the dplyr package
df.p <- df.trial %>%
  filter(game > 1 & pred.EV != pred.RSF) %>%
  group_by(id, `Goal Condition`, `Variance Condition`) %>%
  summarise(
    pred.EV.acc.rate = mean(pred.EV.acc, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc, na.rm = TRUE),
    ped.diff.RST.m.EV = pred.RSF.acc.rate - pred.EV.acc.rate)

pdf("plot/predictionAccuraciesDiffRST-EV.pdf", width = 12, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(ped.diff.RST.m.EV ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy RST - EV\n", xlab = "Condition", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(-1, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
abline(h = 0, lty = 2, lwd = 3, col = transparent("darkred", .6))
dev.off()

pdf("plot/predictionAccuraciesDiffPred.pdf", width = 12, height = 10.5)

par(mfrow = c(2, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(pred.RSF.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "RST",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "EV",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()




### histogram of trials when model predictions differ -------

# add histogram to show in which trial to what proportion the models differ

pdf("plot/histTrialsDiffPred.pdf", width = 9, height = 5.5)
par(mfrow = c(1, 1), mar = c(5,6.5,3,1.5))
temp.df <- subset(df.trial, game > 1 & pred.EV != pred.RSF)
hist(temp.df$trial, probability = TRUE, breaks = 25, ylim = c(0, 0.1),
     col = gray(0.3, 0.5), xlim = c(0,25), xlab = "Trial Number", main = "",
     las = 1, cex.axis = 1.2, cex.lab = 1.3, ylab = "Density\n")
dev.off()



temp.df <- df.trial %>% filter(game > 1) %>%
  group_by(trial) %>%
  summarise(prop = length(trial[pred.EV != pred.RSF])/length(trial))

pdf("plot/histTrialsDiffPredProp.pdf", width = 9, height = 5.5)
par(mfrow = c(1, 1), mar = c(5,6.5,3,1.5))
plot(temp.df$trial, temp.df$prop,
     ylim = c(0, 1), type = "h", lwd = 10,
     col = gray(0.3, 0.5), xlim = c(0,25), xlab = "Trial Number", main = "",
     las = 1, cex.axis = 1.2, cex.lab = 1.3, ylab = "Proportion Differing Predictions\n")
dev.off()




# same plot but separated for goal vs no goal condition
temp.df <- df.trial %>% filter(game > 1) %>%
  group_by(trial, `Goal Condition`) %>%
  summarise(prop = length(trial[pred.EV != pred.RSF])/length(trial))

pdf("plot/histTrialsDiffPredPropGoal.pdf", width = 9, height = 5.5)
par(mfrow = c(2, 1), mar = c(5,6.5,3,1.5))
plot(temp.df$trial[temp.df$`Goal Condition` == "Goal"], temp.df$prop[temp.df$`Goal Condition` == "Goal"],
     ylim = c(0, 1), type = "h", lwd = 10,
     col = gray(0.3, 0.5), xlim = c(0,25), xlab = "Trial Number", main = "",
     las = 1, cex.axis = 1.2, cex.lab = 1.3, ylab = "Proportion Differing Prediction\n")
plot(temp.df$trial[temp.df$`Goal Condition` == "NoGoal"], temp.df$prop[temp.df$`Goal Condition` == "NoGoal"],
     ylim = c(0, 1), type = "h", lwd = 10,
     col = gray(0.3, 0.5), xlim = c(0,25), xlab = "Trial Number", main = "",
     las = 1, cex.axis = 1.2, cex.lab = 1.3, ylab = "Proportion Differing Prediction\n")
dev.off()




### check point values when predictions differed vs where the same ----
temp.df <- df.trial %>% filter(game > 1) %>%
  mutate(diff.pred = case_when(pred.EV == pred.RSF ~ 0,
                               pred.EV != pred.RSF ~ 1)) %>%
  group_by(`Variance Condition`, `Goal Condition`, id, diff.pred) %>%
  summarise(points.cum.median = median(points.cum, na.rm = TRUE))
names(temp.df)[4] <- "Differing Predictions"


pdf("plot/medPointValsDiffPred.pdf", width = 12, height = 10.5)
par(mfrow = c(2, 1), mar = c(5,10,3,1.5))
yarrr::pirateplot(points.cum.median ~ `Differing Predictions` + `Variance Condition`, data = subset(temp.df, `Goal Condition` == "Goal"),
                  ylab = "Median Points\n", xlab = "Condition", main = "Goal",
                  bean.f.col = c("lightgray", "black"), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(points.cum.median ~ `Differing Predictions` + `Variance Condition`, data = subset(temp.df, `Goal Condition` == "NoGoal"),
                  ylab = "Median Points\n", xlab = "Condition", main = "No Goal",
                  bean.f.col = c("lightgray", "black"), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()

# numeracy -------

df.participant$BNTacc <- ifelse(df.participant$BNT %in% c("25", 25, "25%"), 1, ifelse(df.participant$BNT %in% c("fuck off", "x"), NA, 0))
temp.df <- subset(df.participant, knewBNT == 2)
table(temp.df$BNTacc)

par(mfrow = c(2, 1), mar = c(5,10,3,1.5))
yarrr::pirateplot(pred.EV.acc.rate ~ BNTacc + `Variance Condition`,
                  data = subset(df.participant, `Goal Condition` == "Goal"),
                  ylab = "Median Points\n", xlab = "Condition", main = "Goal",
                  bean.f.col = c("lightgray", "black"), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.rate ~ BNTacc + `Variance Condition`,
                  data = subset(df.participant, `Goal Condition` == "No Goal"),
                  ylab = "Median Points\n", xlab = "Condition", main = "No Goal",
                  bean.f.col = c("lightgray", "black"), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)


### Environments distributions --------------


pdf("plot/Environments.pdf", width = 9, height = 12)
par(mfrow = c(3, 1), mar = c(5,6.5,3,1.5))

# Equal
x <- seq(-30, 40, length=100)
hx <- dnorm(x, 4, 2.5)


plot(x, hx, type="l", lty=2, xlab="Outcome",
     ylab="Density\n", main="Equal Environment", lwd = 3, bty = "l",
     las = 1, cex.axis = 1.2, cex.lab = 1.4, yaxs = "i", ylim = c(0, .17))

x <- seq(-30, 40, length=100)
hx <- dnorm(x, 4, 11) 
lines(x, hx, lwd=3, lty = 3)

abline(v = 4, lty = 4, lwd = 3, col = gray(.6, .5))


legend("right", inset=.05, title="",
       c("Option A", "Option B"), lwd=3, lty=c(2, 3), cex = 1.4,
       bty = "n")


# High
x <- seq(-30, 40, length=100)
hx <- dnorm(x, 2.5, 2.5)


plot(x, hx, type="l", lty=2, xlab="Outcome",
     ylab="Density\n", main="High Environment", lwd = 3, bty = "l",
     las = 1, cex.axis = 1.2, cex.lab = 1.4, yaxs = "i", ylim = c(0, .17))

x <- seq(-30, 40, length=100)
hx <- dnorm(x, 4, 11) 
lines(x, hx, lwd=3, lty = 3)

abline(v = 4, lty = 3, lwd = 3, col = gray(.6, .5))
abline(v = 2.5, lty = 2, lwd = 3, col = gray(.6, .5))


legend("right", inset=.05, title="",
       c("Option A", "Option B"), lwd=3, lty=c(2, 3), cex = 1.4,
       bty = "n")


# Low
x <- seq(-30, 40, length=100)
hx <- dnorm(x, 4, 2.5)


plot(x, hx, type="l", lty=2, xlab="Outcome",
     ylab="Density\n", main="Low Environment", lwd = 3, bty = "l",
     las = 1, cex.axis = 1.2, cex.lab = 1.4, yaxs = "i", ylim = c(0, .17))

x <- seq(-30, 40, length=100)
hx <- dnorm(x, 2.5, 11) 
lines(x, hx, lwd=3, lty = 3)

abline(v = 2.5, lty = 3, lwd = 3, col = gray(.6, .5))
abline(v = 4, lty = 2, lwd = 3, col = gray(.6, .5))


legend("right", inset=.05, title="",
       c("Option A", "Option B"), lwd=3, lty=c(2, 3), cex = 1.4,
       bty = "n")

dev.off()


### Evironments point estimates ------------------
pdf("plot/EnvironmentsPointEstimates.pdf", width = 8, height = 7)
par(mfrow = c(1, 1), mar = c(5,6.5,3,1.5))
par(mar = c(5, 5, 3, 1))
plot(c(11, 2.5, 11, 2.5), c(4, 4, 2.5, 2.5), xlim = c(0, 15), ylim = c(0, 6),
     xlab = "Standard Deviation", ylab = "Mean", bty = "l", pch = c(0, 1, 15, 16),
     col = "black", cex = 3, xaxs = "i", yaxs = "i", cex.lab = 1.5, las = 1,
     cex.axis = 1.2, cex.lab = 1.4)
legend("topleft", legend = c("risky Equal, High", "risky Low", "save Equal, Low", "save High"),
       col = "black", pch = c(0, 15, 1, 16), cex = 1.5, bty = "n")

dev.off()


### States Plot -----------------------------------

pdf("plot/rstStates.pdf")
par(mar=c(1,1,1,1))
plot(1,type = "n", xaxt = "n", yaxt = "n", ylim = c(0, 1),
     xlim = c(0, 1), xaxs = "i", yaxs = "i", ylab = "", xlab = "")

segments(0.1, 0.8, 0.7, 0.8, lty = 2, lwd = 2)

segments(0.1, 0.4, 0.7, 0.4, lty = 2, lwd = 2)

segments(0.1, 0.28, 0.7, 0.28, lty = 2, lwd = 2)

arrows(0.25, 0.5, 0.25, 0.85, lwd = 2)

arrows(0.25, 0.5, 0.25, 0.15, lwd = 2)

arrows(0.55, 0.55, 0.55, 0.65, lwd = 2)

arrows(0.55, 0.55, 0.55, 0.45, lwd = 2)

points(c(0.25, 0.55), c(0.5, 0.55), cex = 5, pch = 21,
       col = "black", bg = c("black", "white"))

text(0.72, 0.8, "Goal state\n(high need)", pos = 4, cex = 2)
text(0.72, 0.4, "Goal state\n(low need)", pos = 4, cex = 2)
text(0.72, 0.28, "\nPresent\nstate", pos = 4, cex = 2)

text(0.25, 0.1, "High-risk\noption", cex = 2)
text(0.55, 0.1, "Low-risk\noption", cex = 2)
dev.off()

### Multiple States Plots -----------------------------------

pdf("plot/rstMultipStates.pdf", width = 15, height = 7)
par(mar=c(1,3,1,1), mfrow = c(1, 2))
plot(1,type = "n", xaxt = "n", yaxt = "n", ylim = c(0, 1),
     xlim = c(0, 1), xaxs = "i", yaxs = "i", ylab = "", xlab = "")

segments(0.1, 0.8, 0.7, 0.8, lty = 2, lwd = 2, col = gray(.25, .7))

segments(0.1, 0.4, 0.7, 0.4, lty = 2, lwd = 2, col = gray(.25, .7))

segments(0.1, 0.28, 0.7, 0.28, lty = 2, lwd = 2, col = gray(.25, .7))

arrows(0.25, 0.5, 0.25, 0.85, lwd = 2)

arrows(0.25, 0.5, 0.25, 0.15, lwd = 2)

arrows(0.55, 0.60, 0.55, 0.70, lwd = 2)

arrows(0.55, 0.60, 0.55, 0.50, lwd = 2)

points(c(0.25, 0.55), c(0.5, 0.60), cex = 5, pch = 21,
       col = "black", bg = c("black", "white"))

text(0.72, 0.8, "Goal state\n(high need)", pos = 4, cex = 2)
text(0.72, 0.4, "Goal state\n(low need)", pos = 4, cex = 2)
text(0.72, 0.28, "\nPresent\nstate", pos = 4, cex = 2)

text(0.25, 0.1, "High-risk\noption", cex = 2)
text(0.55, 0.1, "Low-risk\noption", cex = 2)

mtext("a.", at = c( .95), side = 2, adj = 1, las = 1, cex = 2.5, line = 1)



plot(1,type = "n", xaxt = "n", yaxt = "n", ylim = c(0, 1),
     xlim = c(0, 1), xaxs = "i", yaxs = "i", ylab = "", xlab = "")

#segments(0.1, 0.8, 0.7, 0.8, lty = 2, lwd = 2)

segments(0.1, 0.4, 0.7, 0.4, lty = 2, lwd = 2, col = gray(.25, .7))

segments(0.1, 0.28, 0.7, 0.28, lty = 2, lwd = 2, col = gray(.25, .7))

arrows(0.25, 0.55, 0.25, 0.90, lwd = 2)

arrows(0.25, 0.55, 0.25, 0.20, lwd = 2)

arrows(0.55, 0.45, 0.55, 0.55, lwd = 2)

arrows(0.55, 0.45, 0.55, 0.35, lwd = 2)

points(c(0.25, 0.55), c(0.55, 0.45), cex = 5, pch = 21,
       col = "black", bg = c("black", "white"))

#text(0.72, 0.8, "Goal state\n(high need)", pos = 4, cex = 2)
text(0.72, 0.4, "\nPresent\nstate", pos = 4, cex = 2)
text(0.72, 0.28, "Goal state", pos = 4, cex = 2)

text(0.25, 0.1, "High-risk\noption", cex = 2)
text(0.55, 0.1, "Low-risk\noption", cex = 2)
mtext("b.", at = c( .95), side = 2, adj = 1, las = 1, cex = 2.5, line = 1)


dev.off()
