rm(list = ls())
gc()

### model comparison SampEx models

#load and rename data
df.participant <- readRDS("data/Study1Data/useData/S1_dataParticipantLevel.rds")

# SampExHeur no goal model
load("data/Study1Data/useData/SampExHeur_noGoal_Fits.RData")

df.participant$gsq_SampEx_noGoal <- r_good_SampEx
df.participant$corrPred_SampEx_noGoal <- r_pred_SampEx
df.participant$phi_SampEx_noGoal <- r_parr_SampEx[, 2]
df.participant$N_SampEx_noGoal <- r_parr_SampEx[, 1]

# SampExHeur goal model
load("data/Study1Data/useData/SampExHeur_Fits.RData")

df.participant$gsq_SampEx <- r_good_SampEx
df.participant$corrPred_SampEx <- r_pred_SampEx
df.participant$phi_SampEx <- r_parr_SampEx[, 2]
df.participant$N_SampEx <- r_parr_SampEx[, 1]

# reinforcement learning model
load("data/Study1Data/useData/RL_Fits.RData")

df.participant$gsq_RL <- r_good_RL
df.participant$corrPred_RL <- r_pred_RL
df.participant$phi_RL <- r_parr_RL[, 2]
df.participant$alpha_RL <- r_parr_RL[, 1]


mean(df.participant$gsq_SampEx < df.participant$gsq_SampEx_noGoal)
tapply(df.participant$gsq_SampEx < df.participant$gsq_SampEx_noGoal,
       list(df.participant$variance.condition,
            df.participant$goal.condition), mean, na.rm = T)

tapply(df.participant$corrPred_RL,
       list(df.participant$variance.condition,
            df.participant$goal.condition), mean, na.rm = T)

tapply(df.participant$gsq_SampEx < df.participant$gsq_RL,
       list(df.participant$variance.condition,
            df.participant$goal.condition), mean, na.rm = T)

# check parameters

hist(df.participant$phi_SampEx)

plot(df.participant$alpha_RL, df.participant$phi_RL)

plot(df.participant$N_SampEx, df.participant$phi_SampEx)

plot(df.participant$N_SampEx_noGoal, df.participant$phi_SampEx_noGoal)

# plot correct predictions
# Plot prediction accuracy separater for conditions ---------------
names(df.participant)[c(3:4, 32)] <- c("Goal Condition", "Variance Condition",
                                       "Indicated Strategy")
df.participant$`Goal Condition`[df.participant$`Goal Condition` == "NoGoal"] <- "No Goal"
df.participant$`Indicated Strategy`[df.participant$`Indicated Strategy` == 1] <- "EV"
df.participant$`Indicated Strategy`[df.participant$`Indicated Strategy` == 2] <- "RSF"

pdf("plot/predAccuracyPirateSampExRL.pdf", width = 12, height = 15.5)
par(mar=c(5,9.5,3,1.5), mfrow = c(3, 1))

yarrr::pirateplot(corrPred_SampEx ~ `Goal Condition` + `Variance Condition`,
                  data = df.participant,
                  main = "SampExHeur Model", ylab = "SampEx Prediction Accuray", 
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(corrPred_SampEx_noGoal ~ `Goal Condition` + `Variance Condition`,
                  data = df.participant,
                  main = "SampExHeur No Goal Model",
                  ylab = "SampEx No Goal Prediction Accuracy",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)
yarrr::pirateplot(corrPred_RL ~ `Goal Condition` + `Variance Condition`,
                  data = df.participant,
                  main = "RL Model",
                  ylab = "RL Prediction Accuracy",
                  bean.f.col = c("lightgray", "black"), ylim = c(0, 1), cex.lab = 1.3,
                  cex.axis = 1.3, cex.names = 1.3)


dev.off()


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
pdf("plot/predAccuracySampEx.pdf", width = 12, height = 8)

par(mfrow = c(2, 3), mar = c(5.1, 6.5, 3, 1))

for (i in seq_len(6)){
  plot(1, xlim = c(0, 1.05), ylim = c(0, 1.05), pch = 16, xlab = "SampEx No Goal",
       ylab = "SampEx Goal", xaxs = 'i', yaxs = 'i', las = 1, cex.axis = 1.5,
       cex.lab = 1.75, type = "n")
  
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
  with(df.participant, points(corrPred_SampEx_noGoal[`Goal Condition` == goals[i] &
                                                 `Variance Condition` == envs[i]],
                              corrPred_SampEx[`Goal Condition` == goals[i] &
                                                  `Variance Condition` == envs[i]],
                              xlim = c(0, 1), ylim = c(0, 1), pch = 16,
                              col = gray(0, .6)))
  abline(0, 1, lty = 2)
  
}
dev.off()



