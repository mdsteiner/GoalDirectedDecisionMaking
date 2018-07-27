# compare fits from optim and stan

load("data/Study1Data/useData/ModelFitOptim01.RData")


optim_fits <- subj_fits[subj_fits$id %in% ids & subj_fits$model == "RL",]

fit_df <- as.data.frame(fit)
plot(colMeans(fit_df[,1:30]), optim_fits$pars_Imp_mle,
     xlab = "stan fit", ylab = "optim fit", main = "alpha parameters",
     ylim = c(0, 1), xlim = c(0, 1), pch = 16)
abline(0, 1)

plot(colMeans(fit_df[,31:60]), optim_fits$pars_Choice_mle,
     xlab = "stan fit", ylab = "optim fit", main = "phi parameters",
     ylim = c(0, 1), xlim = c(0, 1), pch = 16)
