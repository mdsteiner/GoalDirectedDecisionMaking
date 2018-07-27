// rl model with rescorla wagner function and softmax function

data{
  
  // number of participants
  int<lower = 1> N;
  
  // number of games
  int<lower = 1> n_games;
  
  // number of trials
  int<lower = 1> n_trials;
  
  // array with outcomes
  real outcomes[n_games * n_trials, N];
  
  // choice data
  int<lower = 0, upper = 1> choices[n_games * n_trials, N];
  
}

transformed data{
    vector[2] imp_init;              // Initial impression values
    imp_init  = rep_vector(0, 2);   // Initial impression values
}

parameters{
  
  // learning rate
  vector<lower=0, upper=1>[N] alpha;
  
  // choice sensitivity parameter
  vector<lower=0, upper=5>[N] phi;
  
}

/*
transformed parameters {
  // trials_maxransform subject-level raw parameters 
  vector<lower=0, upper=1>[N] alpha;
  //vector<lower=0, upper=5>[N] phi;

  for (i in 1:N){
    
  alpha[i] = Phi_approx(alpha_pr[N]);
//  phi[i] = Phi_approx(phi_pr[N]) * 5;
  
  }

}

*/
model{
  
  alpha ~ normal(0.5, 0.5);
  phi ~ cauchy(0, 1.0);
  
  for (sub in 1:N){
    
    
    for (gam in 1:n_games){
      
      real p_opt_a;
      vector[2] current_imp;
      current_imp = imp_init;
      
      for (tri in 1:n_trials){
        
        int ind;
        if (tri != 1){
          
          ind = choices[(gam - 1) * n_trials + tri - 1, sub] + 1;
          
          current_imp[ind] = (1 - alpha[sub]) * current_imp[ind] + alpha[sub] *
                             outcomes[(gam - 1) * n_trials + tri - 1, sub];
            
            p_opt_a = exp(phi[sub] * current_imp[ind]) / (exp(phi[sub] * current_imp[1]) + exp(phi[sub] * current_imp[2]));
          
        } else {
          p_opt_a = 0.5;
        }
        
       
      
       choices[(gam - 1) * n_trials + tri, sub] ~ bernoulli(p_opt_a);
      
       // choices[(gam - 1) * n_trials + tri, sub] ~ bernoulli_logit(0.5 * (current_imp[1] -
      //                      current_imp[2]));
        
      }
      
    }
    
  }
  
}

