data {
  int nGoal;
  int rstGoal;
  
  int nNoGoal;
  int rstNoGoal;
}

parameters {
  real<lower=0, upper=1> rateGoal;
  real<lower=0, upper=1> rateNoGoal;
}

model {
  rateGoal ~ normal(0.5, 0.5);
  rateNoGoal ~ normal(0.5, 0.5);
  rstGoal ~ binomial(nGoal, rateGoal);
  rstNoGoal ~ binomial(nNoGoal, rateNoGoal);
}
