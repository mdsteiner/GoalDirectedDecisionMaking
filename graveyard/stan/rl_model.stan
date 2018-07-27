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

parameters{
  
  // learning rate
  vector[N] alpha_pr;
  
  // choice sensitivity parameter
  vector[N] phi_pr;
  
}

transformed parameters {
  // trials_maxransform subject-level raw parameters 
  vector<lower=0, upper=1>[N] alpha;
  vector<lower=0, upper=20>[N] phi;

  for (i in 1:N) {
    alpha[i] = Phi_approx(alpha_pr[i]);
    phi[i] = Phi_approx(phi_pr[i]) * 20;
  }
}


model{
  
  alpha_pr ~ normal(0, 5.0);
  phi_pr ~ normal(0, 5.0);
  
  
  for (sub in 1:N){
      int ind;
    
    
    for (gam in 1:n_games){
      
      real current_imp[2];
      
      current_imp[1] = 0;
      current_imp[2] = 0;
      
      
      for (tri in 1:n_trials){
        
        if (tri != 1){
          
            ind = choices[(gam - 1) * 25 + tri - 1, sub] + 1;
            current_imp[ind] = (1 - alpha[sub]) * current_imp[ind] + alpha[sub] * outcomes[(gam - 1) * 25 + tri - 1, sub];
            
          
        }
        
        choices[(gam - 1) * 25 + tri, sub] ~ bernoulli_logit(phi[sub] * (current_imp[1] - current_imp[2]));
        
      }
      
    }
    
  }
  
}

/*
generated quantities {
    
  real log_lik[N];

  
  {
    for (sub in 1:N){
      
      log_lik[sub] = 0;
      
      for (gam in 1:n_games){
      
        real current_imp_0;
        real current_imp_1;
        real p_opt_0; // probability of choosing option 0 over option 1
        
        current_imp_0 = 0;
        current_imp_1 = 0;
        
        
        for (tri in 1:n_trials){
          
          if (tri != 1){
            
            if (choices[(gam - 1) * 25 + tri - 1, sub] == 0){
              
              current_imp_0 = 1 - alpha[sub] * current_imp_0 + alpha[sub] * outcomes[(gam - 1) * 25 + tri - 1, sub];
              
            } else {
              
              current_imp_1 = 1 - alpha[sub] * current_imp_1 + alpha[sub] * outcomes[(gam - 1) * 25 + tri - 1, sub];
              
            }
            
          }
          
          p_opt_0 = inv_logit( phi[sub] * (current_imp_0 - current_imp_1) );
          log_lik[sub] = log_lik[sub] + bernoulli_lpmf( choices[(gam - 1) * 25 + tri, sub] | p_opt_0 );
          
        }
        
      }
      
    }
    
  }

}
*/

