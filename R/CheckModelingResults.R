# get modeling results

load("data/Study1Data/useData/model_comparison01.RData")

# Table of results
table(model_best$model_best, model_best$goal.condition)


mean(model_best$model_best_Imp[model_best$model_best == "SampEx_Heur_Goal"])
mean(model_best$model_best_Imp[model_best$model_best == "SampEx_Heur_NoGoal"])
