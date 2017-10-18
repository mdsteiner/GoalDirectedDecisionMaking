# Plots for EE-Goals Paper

if (!require(yarrr)) install.packages("yarrr"); library(yarrr)
if (!require(lme4)) install.packages("lme4"); library(lme4)
if (!require(afex)) install.packages("afex"); library(afex)
if (!require(coin)) install.packages("coin"); library(coin)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)

# Set working directory
setwd(rprojroot::is_rstudio_project$find_file())

# load dataframes

df.trial.eu <- readRDS("data/Study1Data/useData/S1_dataTrialLevelEU.rds")
df.participant.eu <- readRDS("data/Study1Data/useData/S1_dataParticipantLevelEU.rds")


names(df.trial.eu)[c(17:18, 26, 41, 44, 63, 66, 69)] <- c("Goal Condition", "Variance Condition",
                                              "Choose Risky", "Choose Risky EU",
                                              "Choose Risky PT", "Choose Risky EV RL",
                                              "Choose Risky EU RL", "Choose Risky PT RL")

df.n <- aggregate(high.var.chosen ~ `Choose Risky EU` + id + `Goal Condition` + `Variance Condition`,
                  FUN = mean, data = subset(df.trial.eu, game > 1))
df.n$`Goal Condition`[df.n$`Goal Condition` == "NoGoal"] <- "No Goal"

pdf("plot/pRiskyChooseRiskyEU.pdf", width = 12, height = 10.5)

par(mar=c(5,9.5,3,1.5), mfrow = c(2,1))

yarrr::pirateplot(high.var.chosen ~ `Choose Risky EU` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

yarrr::pirateplot(high.var.chosen ~ `Choose Risky EU` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "No Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "No Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

dev.off()

df.n <- aggregate(high.var.chosen ~ `Choose Risky PT` + id + `Goal Condition` + `Variance Condition`,
                  FUN = mean, data = subset(df.trial.eu, game > 1))
df.n$`Goal Condition`[df.n$`Goal Condition` == "NoGoal"] <- "No Goal"

pdf("plot/pRiskyChooseRiskyPT.pdf", width = 12, height = 10.5)

par(mar=c(5,9.5,3,1.5), mfrow = c(2,1))

yarrr::pirateplot(high.var.chosen ~ `Choose Risky PT` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

yarrr::pirateplot(high.var.chosen ~ `Choose Risky PT` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "No Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "No Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

dev.off()


## Reinforcement learning methods

# EV
df.n <- aggregate(high.var.chosen ~ `Choose Risky EV RL` + id + `Goal Condition` + `Variance Condition`,
                  FUN = mean, data = subset(df.trial.eu, game > 1))
df.n$`Goal Condition`[df.n$`Goal Condition` == "NoGoal"] <- "No Goal"

pdf("plot/pRiskyChooseRiskyEVRL.pdf", width = 12, height = 10.5)

par(mar=c(5,9.5,3,1.5), mfrow = c(2,1))

yarrr::pirateplot(high.var.chosen ~ `Choose Risky EV RL` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

yarrr::pirateplot(high.var.chosen ~ `Choose Risky EV RL` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "No Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "No Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

dev.off()



df.n <- aggregate(high.var.chosen ~ `Choose Risky EU RL` + id + `Goal Condition` + `Variance Condition`,
                  FUN = mean, data = subset(df.trial.eu, game > 1))
df.n$`Goal Condition`[df.n$`Goal Condition` == "NoGoal"] <- "No Goal"

pdf("plot/pRiskyChooseRiskyEURL.pdf", width = 12, height = 10.5)

par(mar=c(5,9.5,3,1.5), mfrow = c(2,1))

yarrr::pirateplot(high.var.chosen ~ `Choose Risky EU RL` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

yarrr::pirateplot(high.var.chosen ~ `Choose Risky EU RL` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "No Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "No Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

dev.off()

df.n <- aggregate(high.var.chosen ~ `Choose Risky PT RL` + id + `Goal Condition` + `Variance Condition`,
                  FUN = mean, data = subset(df.trial.eu, game > 1))
df.n$`Goal Condition`[df.n$`Goal Condition` == "NoGoal"] <- "No Goal"

pdf("plot/pRiskyChooseRiskyPTRL.pdf", width = 12, height = 10.5)

par(mar=c(5,9.5,3,1.5), mfrow = c(2,1))

yarrr::pirateplot(high.var.chosen ~ `Choose Risky PT RL` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

yarrr::pirateplot(high.var.chosen ~ `Choose Risky PT RL` + `Variance Condition`,
                  data = subset(df.n, `Goal Condition` == "No Goal"),
                  ylab = "p Risky chosen", xlab = "",
                  main = "No Goal", bean.f.col = c("lightgray", "black"), ylim = c(0, 1),
                  cex.lab = 1.3, cex.axis = 1.3, cex.names = 1.3)

dev.off()


# Plot evidence strength EU-----------------------------

evidence_strength2 <- function(df, strategy, nround, nbatches, withText,
                               returnDf, cex.m, useLines, cex.text, whichType, ...){
  if (strategy == "RST") {
    fav.opt <- round(df$p.getthere.1.subj.eu - df$p.getthere.2.subj.eu, nround)
  } else if (strategy == "EV") {
    fav.opt <- round((df$subj.mean.1.eu / df$sd.sub1) - (df$subj.mean.2.eu / df$sd.sub2), nround)
  } else {
    stop("No valid strategy. Strategy must be either EV or RST")
  }
  fav.opt[fav.opt == Inf | fav.opt == -Inf] <- NA
  
  # create batches
  batch.size <- (max(fav.opt, na.rm = TRUE) - min(fav.opt, na.rm = TRUE)) / nbatches
  
  batch.int <- cumsum(c(min(fav.opt, na.rm = TRUE), rep(batch.size, nbatches)))
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  p.choose.1 <- unlist(lapply(seq_len(nbatches),
                              function(x, df, fav.opt, batch.int){
                                mean(df$selection[fav.opt >= batch.int[x] &
                                                    fav.opt < batch.int[x+1]] == 1,
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
    lines(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
          ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  } else {
    plot(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
         ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  }
  
  if (isTRUE(withText)){
    text(x = batch.int[1:(length(batch.int)-1)] + (batch.size / 2), 
         y = p.choose.1, 
         labels = numobs.s, pos = 3,
         cex = cex.text)
  }
  
  if (isTRUE(returnDf)){
    df.evidence <- data.frame("p.choose.1" = p.choose.1,
                              "batch.int" = batch.int[1:(length(batch.int)-1)] + (batch.size / 2),
                              "n.obs" = numobs,
                              "n.obs.s" = numobs.s)
    
    df.evidence
  }
}

pdf("plot/EvidenceStrengthsEU.pdf", width = 10, height = 13.125)
# All conditions
par(mfrow = c(3,1), mar =c(5.1, 7.5, 4.1, 2.1))
# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-1, 1),
                   lty = 2, xlab = "p reach goal A - p reach goal B",
                   main = "Evidence Strength RST, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "RST", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

### EV evidence strength

# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength EV, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)


# Evidence strength EV No Goal


# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength EV, No Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

dev.off()


# Plot evidence strength PT-----------------------------

evidence_strength2 <- function(df, strategy, nround, nbatches, withText,
                               returnDf, cex.m, useLines, cex.text, whichType, ...){
  if (strategy == "RST") {
    fav.opt <- round(df$p.getthere.1.subj.pt - df$p.getthere.2.subj.pt, nround)
  } else if (strategy == "EV") {
    fav.opt <- round((df$subj.mean.1.pt / df$sd.sub1) - (df$subj.mean.2.pt / df$sd.sub2), nround)
  } else {
    stop("No valid strategy. Strategy must be either EV or RST")
  }
  fav.opt[fav.opt == Inf | fav.opt == -Inf] <- NA
  
  # create batches
  batch.size <- (max(fav.opt, na.rm = TRUE) - min(fav.opt, na.rm = TRUE)) / nbatches
  
  batch.int <- cumsum(c(min(fav.opt, na.rm = TRUE), rep(batch.size, nbatches)))
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  p.choose.1 <- unlist(lapply(seq_len(nbatches),
                              function(x, df, fav.opt, batch.int){
                                mean(df$selection[fav.opt >= batch.int[x] &
                                                    fav.opt < batch.int[x+1]] == 1,
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
    lines(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
          ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  } else {
    plot(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
         ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  }
  
  if (isTRUE(withText)){
    text(x = batch.int[1:(length(batch.int)-1)] + (batch.size / 2), 
         y = p.choose.1, 
         labels = numobs.s, pos = 3,
         cex = cex.text)
  }
  
  if (isTRUE(returnDf)){
    df.evidence <- data.frame("p.choose.1" = p.choose.1,
                              "batch.int" = batch.int[1:(length(batch.int)-1)] + (batch.size / 2),
                              "n.obs" = numobs,
                              "n.obs.s" = numobs.s)
    
    df.evidence
  }
}

pdf("plot/EvidenceStrengthsPT.pdf", width = 10, height = 13.125)
# All conditions
par(mfrow = c(3,1), mar =c(5.1, 7.5, 4.1, 2.1))
# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-1, 1),
                   lty = 2, xlab = "p reach goal A - p reach goal B",
                   main = "Evidence Strength RST, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "RST", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

### EV evidence strength

# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength EV, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)


# Evidence strength EV No Goal


# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength EV, No Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

dev.off()

# Plot evidence strength EV RL-----------------------------

evidence_strength2 <- function(df, strategy, nround, nbatches, withText,
                               returnDf, cex.m, useLines, cex.text, whichType, ...){
  if (strategy == "RST") {
    fav.opt <- round(df$p.getthere.1.subj.ev.rl - df$p.getthere.2.subj.ev.rl, nround)
  } else if (strategy == "EV") {
    fav.opt <- round((df$subj.mean.1.ev.rl / df$sd.sub1) - (df$subj.mean.2.ev.rl / df$sd.sub2), nround)
  } else {
    stop("No valid strategy. Strategy must be either EV or RST")
  }
  fav.opt[fav.opt == Inf | fav.opt == -Inf] <- NA
  
  # create batches
  batch.size <- (max(fav.opt, na.rm = TRUE) - min(fav.opt, na.rm = TRUE)) / nbatches
  
  batch.int <- cumsum(c(min(fav.opt, na.rm = TRUE), rep(batch.size, nbatches)))
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  p.choose.1 <- unlist(lapply(seq_len(nbatches),
                              function(x, df, fav.opt, batch.int){
                                mean(df$selection[fav.opt >= batch.int[x] &
                                                    fav.opt < batch.int[x+1]] == 1,
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
    lines(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
          ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  } else {
    plot(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
         ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  }
  
  if (isTRUE(withText)){
    text(x = batch.int[1:(length(batch.int)-1)] + (batch.size / 2), 
         y = p.choose.1, 
         labels = numobs.s, pos = 3,
         cex = cex.text)
  }
  
  if (isTRUE(returnDf)){
    df.evidence <- data.frame("p.choose.1" = p.choose.1,
                              "batch.int" = batch.int[1:(length(batch.int)-1)] + (batch.size / 2),
                              "n.obs" = numobs,
                              "n.obs.s" = numobs.s)
    
    df.evidence
  }
}

pdf("plot/EvidenceStrengthsEVRL.pdf", width = 10, height = 13.125)
# All conditions
par(mfrow = c(3,1), mar =c(5.1, 7.5, 4.1, 2.1))
# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-1, 1),
                   lty = 2, xlab = "p reach goal A - p reach goal B",
                   main = "Evidence Strength RST RL, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "RST", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

### EV evidence strength

# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength EV RL, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)


# Evidence strength EV No Goal


# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength EV RL, No Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

dev.off()



# Plot evidence strength EU-----------------------------

evidence_strength2 <- function(df, strategy, nround, nbatches, withText,
                               returnDf, cex.m, useLines, cex.text, whichType, ...){
  if (strategy == "RST") {
    fav.opt <- round(df$p.getthere.1.subj.eu.rl - df$p.getthere.2.subj.eu.rl, nround)
  } else if (strategy == "EV") {
    fav.opt <- round((df$subj.mean.1.eu.rl / df$sd.sub1) - (df$subj.mean.2.eu.rl / df$sd.sub2), nround)
  } else {
    stop("No valid strategy. Strategy must be either EV or RST")
  }
  fav.opt[fav.opt == Inf | fav.opt == -Inf] <- NA
  
  # create batches
  batch.size <- (max(fav.opt, na.rm = TRUE) - min(fav.opt, na.rm = TRUE)) / nbatches
  
  batch.int <- cumsum(c(min(fav.opt, na.rm = TRUE), rep(batch.size, nbatches)))
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  p.choose.1 <- unlist(lapply(seq_len(nbatches),
                              function(x, df, fav.opt, batch.int){
                                mean(df$selection[fav.opt >= batch.int[x] &
                                                    fav.opt < batch.int[x+1]] == 1,
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
    lines(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
          ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  } else {
    plot(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
         ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  }
  
  if (isTRUE(withText)){
    text(x = batch.int[1:(length(batch.int)-1)] + (batch.size / 2), 
         y = p.choose.1, 
         labels = numobs.s, pos = 3,
         cex = cex.text)
  }
  
  if (isTRUE(returnDf)){
    df.evidence <- data.frame("p.choose.1" = p.choose.1,
                              "batch.int" = batch.int[1:(length(batch.int)-1)] + (batch.size / 2),
                              "n.obs" = numobs,
                              "n.obs.s" = numobs.s)
    
    df.evidence
  }
}

pdf("plot/EvidenceStrengthsEURL.pdf", width = 10, height = 13.125)
# All conditions
par(mfrow = c(3,1), mar =c(5.1, 7.5, 4.1, 2.1))
# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-1, 1),
                   lty = 2, xlab = "p reach goal A - p reach goal B",
                   main = "Evidence Strength RST EU RL, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "RST", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

### EV evidence strength

# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength EU RL, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)


# Evidence strength EV No Goal


# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength EU RL, No Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

dev.off()


# Plot evidence strength PT RL-----------------------------

evidence_strength2 <- function(df, strategy, nround, nbatches, withText,
                               returnDf, cex.m, useLines, cex.text, whichType, ...){
  if (strategy == "RST") {
    fav.opt <- round(df$p.getthere.1.subj.pt.rl - df$p.getthere.2.subj.pt.rl, nround)
  } else if (strategy == "EV") {
    fav.opt <- round((df$subj.mean.1.pt.rl / df$sd.sub1) - (df$subj.mean.2.pt.rl / df$sd.sub2), nround)
  } else {
    stop("No valid strategy. Strategy must be either EV or RST")
  }
  fav.opt[fav.opt == Inf | fav.opt == -Inf] <- NA
  
  # create batches
  batch.size <- (max(fav.opt, na.rm = TRUE) - min(fav.opt, na.rm = TRUE)) / nbatches
  
  batch.int <- cumsum(c(min(fav.opt, na.rm = TRUE), rep(batch.size, nbatches)))
  
  # compute the probability of choosing option 1, given a certain RSF.diff value
  p.choose.1 <- unlist(lapply(seq_len(nbatches),
                              function(x, df, fav.opt, batch.int){
                                mean(df$selection[fav.opt >= batch.int[x] &
                                                    fav.opt < batch.int[x+1]] == 1,
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
    lines(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
          ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  } else {
    plot(batch.int[1:(length(batch.int)-1)] + (batch.size / 2), p.choose.1, type = whichType,
         ylab = "p of Choosing Option A\n", cex = cex.m * exp(numobs.s), ...)
  }
  
  if (isTRUE(withText)){
    text(x = batch.int[1:(length(batch.int)-1)] + (batch.size / 2), 
         y = p.choose.1, 
         labels = numobs.s, pos = 3,
         cex = cex.text)
  }
  
  if (isTRUE(returnDf)){
    df.evidence <- data.frame("p.choose.1" = p.choose.1,
                              "batch.int" = batch.int[1:(length(batch.int)-1)] + (batch.size / 2),
                              "n.obs" = numobs,
                              "n.obs.s" = numobs.s)
    
    df.evidence
  }
}

pdf("plot/EvidenceStrengthsPTRL.pdf", width = 10, height = 13.125)
# All conditions
par(mfrow = c(3,1), mar =c(5.1, 7.5, 4.1, 2.1))
# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-1, 1),
                   lty = 2, xlab = "p reach goal A - p reach goal B",
                   main = "Evidence Strength RST PT RL, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "RST", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "RST", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

### EV evidence strength

# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength PT RL, Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "Goal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)


# Evidence strength EV No Goal


# Equal 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Equal" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, FALSE, 1.5, "l", col= gray(0.65, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 15, xlim = c(-15, 15),
                   lty = 2, xlab = "d' A - d' B",
                   main = "Evidence Strength PT RL, No Goal Condition", bty = "l",
                   cex.lab = 2, cex.axis = 1.6, cex.main = 1.8, las = 1)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "Low" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.55, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 17,
                   lty = 3)

# High 
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & 
                            `Variance Condition` == "High" & game > 1), "EV", 2, 10, 
                   FALSE, FALSE, 1.5, TRUE, 1.5, "l", col= gray(0.6, .7), lwd = 2.25,
                   ylim = c(0, 1), pch = 18,
                   lty = 4)

# All together
evidence_strength2(subset(df.trial.eu, `Goal Condition` == "NoGoal" & game > 1), "EV", 2, 10, 
                   TRUE, FALSE, 2, TRUE, 1.5, "b", col= gray(0.3, .7), lwd = 3,
                   cex.lab = 1.5, cex.axis = 1.8, ylim = c(0, 1), pch = 16)

legend("right", c("All", "Equal", "High", "Low"), lty = c(1:4),
       pch = c(16, rep(NA, 3)), lwd = c(3, rep(2.25, 3)),
       col = c(gray(0.3, .7), gray(0.65, .7), gray(0.6, .7), gray(0.55, .7)),
       bty = "n", cex = 2)

dev.off()

### Prediction Accuracies Differing predictions EU---------

# change this to show trials in people go with rst and ev, rsp.

# aggregate using the dplyr package
df.p <- df.trial.eu %>%
  filter(game > 1 & pred.EV.eu != pred.RSF.eu) %>%
  group_by(id, `Goal Condition`, `Variance Condition`) %>%
  summarise(
    pred.EV.acc.rate = mean(pred.EV.acc.eu, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc.eu, na.rm = TRUE),
    ped.diff.RST.m.EV = pred.RSF.acc.rate - pred.EV.acc.rate)

pdf("plot/predictionAccuraciesDiffRST-EVEU.pdf", width = 12, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(ped.diff.RST.m.EV ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy RST - EU\n", xlab = "Condition", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(-1, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
abline(h = 0, lty = 2, lwd = 3, col = transparent("darkred", .6))
dev.off()

pdf("plot/predictionAccuraciesDiffPredEU.pdf", width = 12, height = 10.5)

par(mfrow = c(2, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(pred.RSF.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "RST",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "EU",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()


### Prediction Accuracies Differing predictions PT---------

# change this to show trials in people go with rst and ev, rsp.

# aggregate using the dplyr package
df.p <- df.trial.eu %>%
  filter(game > 1 & pred.EV.pt != pred.RSF.pt) %>%
  group_by(id, `Goal Condition`, `Variance Condition`) %>%
  summarise(
    pred.EV.acc.rate = mean(pred.EV.acc.pt, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc.pt, na.rm = TRUE),
    ped.diff.RST.m.EV = pred.RSF.acc.rate - pred.EV.acc.rate)

pdf("plot/predictionAccuraciesDiffRST-EVPT.pdf", width = 12, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(ped.diff.RST.m.EV ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy RST - PT\n", xlab = "Condition", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(-1, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
abline(h = 0, lty = 2, lwd = 3, col = transparent("darkred", .6))
dev.off()

pdf("plot/predictionAccuraciesDiffPredPT.pdf", width = 12, height = 10.5)

par(mfrow = c(2, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(pred.RSF.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "RST",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "PT",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()


### Prediction Accuracies Differing predictions EV RL---------

# change this to show trials in people go with rst and ev, rsp.

# aggregate using the dplyr package
df.p <- df.trial.eu %>%
  filter(game > 1 & pred.EV.ev.rl != pred.RSF.ev.rl) %>%
  group_by(id, `Goal Condition`, `Variance Condition`) %>%
  summarise(
    pred.EV.acc.rate = mean(pred.EV.acc.ev.rl, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc.ev.rl, na.rm = TRUE),
    ped.diff.RST.m.EV = pred.RSF.acc.rate - pred.EV.acc.rate)

pdf("plot/predictionAccuraciesDiffRST-EVEVRL.pdf", width = 12, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(ped.diff.RST.m.EV ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy RST - EV RL\n", xlab = "Condition", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(-1, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
abline(h = 0, lty = 2, lwd = 3, col = transparent("darkred", .6))
dev.off()

pdf("plot/predictionAccuraciesDiffPredEVRL.pdf", width = 12, height = 10.5)

par(mfrow = c(2, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(pred.RSF.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "RST",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "EV RL",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()


### Prediction Accuracies Differing predictions EU RL---------

# change this to show trials in people go with rst and ev, rsp.

# aggregate using the dplyr package
df.p <- df.trial.eu %>%
  filter(game > 1 & pred.EV.eu.rl != pred.RSF.eu.rl) %>%
  group_by(id, `Goal Condition`, `Variance Condition`) %>%
  summarise(
    pred.EV.acc.rate = mean(pred.EV.acc.eu.rl, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc.eu.rl, na.rm = TRUE),
    ped.diff.RST.m.EV = pred.RSF.acc.rate - pred.EV.acc.rate)

pdf("plot/predictionAccuraciesDiffRST-EVEURL.pdf", width = 12, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(ped.diff.RST.m.EV ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy RST - EU RL\n", xlab = "Condition", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(-1, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
abline(h = 0, lty = 2, lwd = 3, col = transparent("darkred", .6))
dev.off()

pdf("plot/predictionAccuraciesDiffPredEURL.pdf", width = 12, height = 10.5)

par(mfrow = c(2, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(pred.RSF.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "RST",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "EU RL",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()


### Prediction Accuracies Differing predictions PT RL---------

# change this to show trials in people go with rst and ev, rsp.

# aggregate using the dplyr package
df.p <- df.trial.eu %>%
  filter(game > 1 & pred.EV.pt.rl != pred.RSF.pt.rl) %>%
  group_by(id, `Goal Condition`, `Variance Condition`) %>%
  summarise(
    pred.EV.acc.rate = mean(pred.EV.acc.pt.rl, na.rm = TRUE),
    pred.RSF.acc.rate = mean(pred.RSF.acc.pt.rl, na.rm = TRUE),
    ped.diff.RST.m.EV = pred.RSF.acc.rate - pred.EV.acc.rate)

pdf("plot/predictionAccuraciesDiffRST-EVPTRL.pdf", width = 12, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(ped.diff.RST.m.EV ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy RST - PT RL\n", xlab = "Condition", main = "",
                  bean.f.col = c("lightgray", "black"), ylim = c(-1, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
abline(h = 0, lty = 2, lwd = 3, col = transparent("darkred", .6))
dev.off()

pdf("plot/predictionAccuraciesDiffPredPTRL.pdf", width = 12, height = 10.5)

par(mfrow = c(2, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(pred.RSF.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "RST",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.rate ~ `Goal Condition` + `Variance Condition`, data = df.p,
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "PT RL",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()
### histogram of trials when model predictions differ -------

# add histogram to show in which trial to what proportion the models differ


temp.df <- df.trial.eu %>% filter(game > 1) %>%
  group_by(trial) %>%
  summarise(prop = length(trial[pred.EV.eu != pred.RSF.eu])/length(trial))

pdf("plot/histTrialsDiffPredPropEU.pdf", width = 9, height = 5.5)
par(mfrow = c(1, 1), mar = c(5,6.5,3,1.5))
plot(temp.df$trial, temp.df$prop,
     ylim = c(0, 1), type = "h", lwd = 10,
     col = gray(0.3, 0.5), xlim = c(0,25), xlab = "Trial Number", main = "",
     las = 1, cex.axis = 1.2, cex.lab = 1.3, ylab = "Proportion Differing Prediction\n")
dev.off()

temp.df <- df.trial.eu %>% filter(game > 1) %>%
  group_by(trial) %>%
  summarise(prop = length(trial[pred.EV.pt != pred.RSF.pt])/length(trial))

pdf("plot/histTrialsDiffPredPropPT.pdf", width = 9, height = 5.5)
par(mfrow = c(1, 1), mar = c(5,6.5,3,1.5))
plot(temp.df$trial, temp.df$prop,
     ylim = c(0, 1), type = "h", lwd = 10,
     col = gray(0.3, 0.5), xlim = c(0,25), xlab = "Trial Number", main = "",
     las = 1, cex.axis = 1.2, cex.lab = 1.3, ylab = "Proportion Differing Prediction\n")
dev.off()




# Plot prediction accuracy separater for conditions  EU---------------
names(df.participant.eu)[3, 4] <- c("Goal Condition", "Variance Condition")
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
pdf("plot/predAccuracyModelComparisonSepCondEU.pdf", width = 12, height = 8)

par(mfrow = c(2, 3), mar = c(5.1, 6.5, 3, 1))

for (i in seq_len(6)){
  plot(1, xlim = c(0, 1.05), ylim = c(0, 1.05), pch = 16, xlab = "EU", ylab = "RST",
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
  with(df.participant.eu, points(pred.EV.acc.eu.rate[`Goal Condition` == goals[i] &
                                                 `Variance Condition` == envs[i]],
                              pred.RSF.acc.eu.rate[`Goal Condition` == goals[i] &
                                                  `Variance Condition` == envs[i]],
                              xlim = c(0, 1), ylim = c(0, 1), pch = 16,
                              col = gray(0, .6)))
  abline(0, 1, lty = 2)
  
}
dev.off()

# Plot prediction accuracy separater for conditions  PT---------------
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
pdf("plot/predAccuracyModelComparisonSepCondPT.pdf", width = 12, height = 8)

par(mfrow = c(2, 3), mar = c(5.1, 6.5, 3, 1))

for (i in seq_len(6)){
  plot(1, xlim = c(0, 1.05), ylim = c(0, 1.05), pch = 16, xlab = "PT", ylab = "RST",
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
  with(df.participant.eu, points(pred.EV.acc.pt.rate[`Goal Condition` == goals[i] &
                                                       `Variance Condition` == envs[i]],
                                 pred.RSF.acc.pt.rate[`Goal Condition` == goals[i] &
                                                        `Variance Condition` == envs[i]],
                                 xlim = c(0, 1), ylim = c(0, 1), pch = 16,
                                 col = gray(0, .6)))
  abline(0, 1, lty = 2)
  
}
dev.off()

# Plot prediction accuracy separater for conditions  EU RL---------------
pdf("plot/predAccuracyModelComparisonSepCondEVRL.pdf", width = 12, height = 8)

par(mfrow = c(2, 3), mar = c(5.1, 6.5, 3, 1))

for (i in seq_len(6)){
  plot(1, xlim = c(0, 1.05), ylim = c(0, 1.05), pch = 16, xlab = "EV RL", ylab = "RST",
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
  with(df.participant.eu, points(pred.EV.acc.ev.rl.rate[`Goal Condition` == goals[i] &
                                                       `Variance Condition` == envs[i]],
                                 pred.RSF.acc.ev.rl.rate[`Goal Condition` == goals[i] &
                                                        `Variance Condition` == envs[i]],
                                 xlim = c(0, 1), ylim = c(0, 1), pch = 16,
                                 col = gray(0, .6)))
  abline(0, 1, lty = 2)
  
}
dev.off()


# Plot prediction accuracy separater for conditions  EU RL---------------
pdf("plot/predAccuracyModelComparisonSepCondEURL.pdf", width = 12, height = 8)

par(mfrow = c(2, 3), mar = c(5.1, 6.5, 3, 1))

for (i in seq_len(6)){
  plot(1, xlim = c(0, 1.05), ylim = c(0, 1.05), pch = 16, xlab = "EU RL", ylab = "RST",
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
  with(df.participant.eu, points(pred.EV.acc.eu.rl.rate[`Goal Condition` == goals[i] &
                                                       `Variance Condition` == envs[i]],
                                 pred.RSF.acc.eu.rl.rate[`Goal Condition` == goals[i] &
                                                        `Variance Condition` == envs[i]],
                                 xlim = c(0, 1), ylim = c(0, 1), pch = 16,
                                 col = gray(0, .6)))
  abline(0, 1, lty = 2)
  
}
dev.off()

# Plot prediction accuracy separater for conditions  PT RL---------------
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
pdf("plot/predAccuracyModelComparisonSepCondPTRL.pdf", width = 12, height = 8)

par(mfrow = c(2, 3), mar = c(5.1, 6.5, 3, 1))

for (i in seq_len(6)){
  plot(1, xlim = c(0, 1.05), ylim = c(0, 1.05), pch = 16, xlab = "PT RL", ylab = "RST",
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
  with(df.participant.eu, points(pred.EV.acc.pt.rl.rate[`Goal Condition` == goals[i] &
                                                       `Variance Condition` == envs[i]],
                                 pred.RSF.acc.pt.rl.rate[`Goal Condition` == goals[i] &
                                                        `Variance Condition` == envs[i]],
                                 xlim = c(0, 1), ylim = c(0, 1), pch = 16,
                                 col = gray(0, .6)))
  abline(0, 1, lty = 2)
  
}
dev.off()


### Prediction Accuracies Barplot EU---------

names(df.participant.eu)[3:4] <- c("Goal Condition", "Variance Condition")
df.participant.eu$`Goal Condition`[df.participant.eu$`Goal Condition` == "NoGoal"] <- "No Goal"

temp.df <- data.frame("Prediction Acc" = c(df.participant.eu$pred.EV.acc.eu.rate,
                                           df.participant.eu$pred.RSF.acc.eu.rate),
                      "Model" = rep(c("EU", "RST"), each = nrow(df.participant.eu)),
                      "Goal Condition" = rep(df.participant.eu$`Goal Condition`, 2))
names(temp.df)[c(1, 3)] <- c("Pred Acc", "Goal Condition")

pdf("plot/predictionAccuraciesBarplotEU.pdf", width = 9, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,8,3,1.5))
pirateplot(`Pred Acc` ~ Model + `Goal Condition`, data = temp.df,
           ylab = "Prediction Accuracy", xlab = "Conditions", main = "",
           bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
           cex.axis = 1.3, cex.names = 1.3)
dev.off()


### Prediction Accuracies Barplot PT---------

names(df.participant.eu)[3:4] <- c("Goal Condition", "Variance Condition")
df.participant.eu$`Goal Condition`[df.participant.eu$`Goal Condition` == "NoGoal"] <- "No Goal"

temp.df <- data.frame("Prediction Acc" = c(df.participant.eu$pred.EV.acc.pt.rate,
                                           df.participant.eu$pred.RSF.acc.pt.rate),
                      "Model" = rep(c("PT", "RST"), each = nrow(df.participant.eu)),
                      "Goal Condition" = rep(df.participant.eu$`Goal Condition`, 2))
names(temp.df)[c(1, 3)] <- c("Pred Acc", "Goal Condition")

pdf("plot/predictionAccuraciesBarplotPT.pdf", width = 9, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,8,3,1.5))
pirateplot(`Pred Acc` ~ Model + `Goal Condition`, data = temp.df,
           ylab = "Prediction Accuracy", xlab = "Conditions", main = "",
           bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
           cex.axis = 1.3, cex.names = 1.3)
dev.off()



### Prediction Accuracies Barplot EV RL---------

names(df.participant.eu)[3:4] <- c("Goal Condition", "Variance Condition")
df.participant.eu$`Goal Condition`[df.participant.eu$`Goal Condition` == "NoGoal"] <- "No Goal"

temp.df <- data.frame("Prediction Acc" = c(df.participant.eu$pred.EV.acc.ev.rl.rate,
                                           df.participant.eu$pred.RSF.acc.ev.rl.rate),
                      "Model" = rep(c("EV RL", "RST"), each = nrow(df.participant.eu)),
                      "Goal Condition" = rep(df.participant.eu$`Goal Condition`, 2))
names(temp.df)[c(1, 3)] <- c("Pred Acc", "Goal Condition")

pdf("plot/predictionAccuraciesBarplotEVRL.pdf", width = 9, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,8,3,1.5))
pirateplot(`Pred Acc` ~ Model + `Goal Condition`, data = temp.df,
           ylab = "Prediction Accuracy", xlab = "Conditions", main = "",
           bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
           cex.axis = 1.3, cex.names = 1.3)
dev.off()


### Prediction Accuracies Barplot EU RL---------

names(df.participant.eu)[3:4] <- c("Goal Condition", "Variance Condition")
df.participant.eu$`Goal Condition`[df.participant.eu$`Goal Condition` == "NoGoal"] <- "No Goal"

temp.df <- data.frame("Prediction Acc" = c(df.participant.eu$pred.EV.acc.eu.rl.rate,
                                           df.participant.eu$pred.RSF.acc.eu.rl.rate),
                      "Model" = rep(c("EU", "RST"), each = nrow(df.participant.eu)),
                      "Goal Condition" = rep(df.participant.eu$`Goal Condition`, 2))
names(temp.df)[c(1, 3)] <- c("Pred Acc", "Goal Condition")

pdf("plot/predictionAccuraciesBarplotEURL.pdf", width = 9, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,8,3,1.5))
pirateplot(`Pred Acc` ~ Model + `Goal Condition`, data = temp.df,
           ylab = "Prediction Accuracy", xlab = "Conditions", main = "",
           bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
           cex.axis = 1.3, cex.names = 1.3)
dev.off()


### Prediction Accuracies Barplot PT RL---------

names(df.participant.eu)[3:4] <- c("Goal Condition", "Variance Condition")
df.participant.eu$`Goal Condition`[df.participant.eu$`Goal Condition` == "NoGoal"] <- "No Goal"

temp.df <- data.frame("Prediction Acc" = c(df.participant.eu$pred.EV.acc.pt.rl.rate,
                                           df.participant.eu$pred.RSF.acc.pt.rl.rate),
                      "Model" = rep(c("PT RL", "RST"), each = nrow(df.participant.eu)),
                      "Goal Condition" = rep(df.participant.eu$`Goal Condition`, 2))
names(temp.df)[c(1, 3)] <- c("Pred Acc", "Goal Condition")

pdf("plot/predictionAccuraciesBarplotPTRL.pdf", width = 9, height = 5.5)

par(mfrow = c(1, 1), mar = c(5,8,3,1.5))
pirateplot(`Pred Acc` ~ Model + `Goal Condition`, data = temp.df,
           ylab = "Prediction Accuracy", xlab = "Conditions", main = "",
           bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
           cex.axis = 1.3, cex.names = 1.3)
dev.off()



### Prediction accuracy separated for indicated strategy EU ----------------------

names(df.participant.eu)[c(3:4, 67)] <- c("Goal Condition", "Variance Condition",
                                       "Indicated Strategy")
df.participant.eu$`Goal Condition`[df.participant.eu$`Goal Condition` == "NoGoal"] <- "No Goal"
df.participant.eu$`Indicated Strategy`[df.participant.eu$`Indicated Strategy` == 1] <- "EU"
df.participant.eu$`Indicated Strategy`[df.participant.eu$`Indicated Strategy` == 2] <- "RST"

pdf("plot/predAccuracyEU.pdf", width = 12, height = 10.5)
par(mar=c(5,9.5,3,1.5), mfrow = c(2, 1))

yarrr::pirateplot(pred.RSF.acc.eu.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "RST Model", ylab = "RST Prediction Accuray", 
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.eu.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "EU Model", ylab = "EU Prediction Accuracy",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()


### Prediction accuracy separated for indicated strategy PT ----------------------


df.participant.eu$`Indicated Strategy`[df.participant.eu$`Indicated Strategy` == "EU"] <- "PT"


pdf("plot/predAccuracyPT.pdf", width = 12, height = 10.5)
par(mar=c(5,9.5,3,1.5), mfrow = c(2, 1))

yarrr::pirateplot(pred.RSF.acc.pt.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "RST Model", ylab = "RST Prediction Accuray", 
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.pt.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "EU Model", ylab = "PT Prediction Accuracy",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()



### Prediction accuracy separated for indicated strategy EV RL ----------------------


df.participant.eu$`Indicated Strategy`[df.participant.eu$`Indicated Strategy` == "PT"] <- "EV RL"


pdf("plot/predAccuracyEVRL.pdf", width = 12, height = 10.5)
par(mar=c(5,9.5,3,1.5), mfrow = c(2, 1))

yarrr::pirateplot(pred.RSF.acc.ev.rl.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "RST Model", ylab = "RST Prediction Accuray", 
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.ev.rl.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "EV RL Model", ylab = "EV RL Prediction Accuracy",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()


### Prediction accuracy separated for indicated strategy EU RL----------------------


df.participant.eu$`Indicated Strategy`[df.participant.eu$`Indicated Strategy` == "EV RL"] <- "EU RL"


pdf("plot/predAccuracyEURL.pdf", width = 12, height = 10.5)
par(mar=c(5,9.5,3,1.5), mfrow = c(2, 1))

yarrr::pirateplot(pred.RSF.acc.eu.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "RST Model", ylab = "RST Prediction Accuray", 
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.eu.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "EU RL Model", ylab = "EU RL Prediction Accuracy",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()

### Prediction accuracy separated for indicated strategy PT RL----------------------

df.participant.eu$`Indicated Strategy`[df.participant.eu$`Indicated Strategy` == "EU RL"] <- "PT RL"


pdf("plot/predAccuracyPTRL.pdf", width = 12, height = 10.5)
par(mar=c(5,9.5,3,1.5), mfrow = c(2, 1))

yarrr::pirateplot(pred.RSF.acc.pt.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "RST Model", ylab = "RST Prediction Accuray", 
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(pred.EV.acc.pt.rate ~ `Indicated Strategy` + `Goal Condition`,
                  data = df.participant.eu,
                  main = "PT RL Model", ylab = "PT RL Prediction Accuracy",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

dev.off()


### Prediction Accuracies Over models---------

# change this to show trials in people go with rst and ev, rsp.

# aggregate using the dplyr package
df.n <- data.frame("Prediction Accuracies" = c(df.participant.eu$pred.EV.acc.rate,
                                               df.participant.eu$pred.RSF.acc.rate,
                                               df.participant.eu$pred.EV.acc.ev.rl.rate,
                                               df.participant.eu$pred.RSF.acc.ev.rl.rate,
                                               df.participant.eu$pred.EV.acc.eu.rl.rate,
                                               df.participant.eu$pred.RSF.acc.eu.rl.rate,
                                               df.participant.eu$pred.EV.acc.pt.rl.rate,
                                               df.participant.eu$pred.RSF.acc.pt.rl.rate),
                   "Model Basis" = rep(c("M", "EV", "EU", "PT"), each = (2*nrow(df.participant.eu))),
                   "Model Version" = rep(c("EV", "RST", "RL", "RST", "RL", "RST", "RL", "RST"),
                                         each = nrow(df.participant.eu)),
                   "goal.condition" = rep(df.participant.eu$goal.condition, 8)
                   )


names(df.n)[2:3] <- c("Model Basis", "Model Version")

pdf("plot/predictionAccuraciesDiffModels.pdf", width = 12, height = 10.5)

par(mfrow = c(2, 1), mar = c(5,9.5,3,1.5))
yarrr::pirateplot(Prediction.Accuracies ~  `Model Version` + `Model Basis`,
                  data = subset(df.n, `Model Basis` != "M" & goal.condition == "Goal"),
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "Goal",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)

yarrr::pirateplot(Prediction.Accuracies ~ `Model Version` + `Model Basis` ,
                  data = subset(df.n, `Model Basis` != "M" & goal.condition == "NoGoal"),
                  ylab = "Prediction Accuracy\n", xlab = "Condition", main = "No Goal",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
dev.off()
