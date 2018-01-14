data{
  
  int<lower = 0> N; // number of subjects
  
  int<lower = 0> ga; // number of games
  
  int<lower = 0> trial; // number of trials
  
  int<lower = 0> max_trial; // maximum number of trials
  
  int<lower = 0> goal; // the goal to be reached
  
  real outc[]; // array containing sampled outcomes
  
  real points[]; // array containing earned points
  
  real choices[]; // array containing choices
  
}

parameters{
  real mu_phi_s; // sampling size mean
  real sigma_phi_s; // sampling size sd
  vector[N] s_phi;
  
  real mu_phi_delta; // forgetting rate
  real sigma_phi_delta;
  vector[N] delta_phi;
  
  real mu_phi_alpha; // learning parameter
  real sigma_phi_alpha;
  vector[N] alpha_phi;
  
  real mu_phi_w; // weight mean and p reach goal
  real sigma_phi_w;
  vector[N] w_phi;
  
  real mu_phi_phi; // choice sensitivity (softmax)
  real sigma_phi_phi;
  vector[N] phi_phi;
}

transformed parameters{
  
  vector[N] s;
  vector[N] delta;
  vector[N] alpha;
  vector[N] w;
  vector[N] phi;
  
  // compute individual parameters
}

model{
  
  // set up hyperparameter priors
  
  
  for (ii in 1:N){
    for (jj in 1:ga){
      
      vector[2] EV;
      
      for (kk in 1:trial){
        
        // evaluate p reach goal
        real s_drawn[s[ii], 2];
        int s_left;
        vector[2] goals_reached;
        int points_left;
        vector[2] V; // value of the options
        
        
        s_drawn[, 1] = draw samples a with probabilities delta; //////////////////////////////////
        s_drawn[, 2] = draw samples b with probabilities delta; //////////////////////////////////
        
        s_left = max_trial - kk; // compute how many trials are left
        
        s_drawn = s_drawn * s_left; // multiply sampled outcomes by number of trials left
        
        points_left = goal - points[kk, jj, ii]; // compute number of points left
        
        goals_reached[1] = mean(s_drawn[,1] >= points_left)
        goals_reached[2] = mean(s_drawn[,2] >= points_left)
        
        EV[choices[kk, jj, ii] + 1] = EV[choices[kk, jj, ii] + 1] + alpha[ii] * (outcomes[kk, jj, ii] - EV[kk, jj, ii]);
        
        V[1] = (EV[1] * w[ii] + goals_reached[1] * (1 - w[ii]) * EV[1]) / 2;
        V[2] = (EV[2] * w[ii] + goals_reached[2] * (1 - w[ii]) * EV[2]) / 2;
        
        choices[kk, jj, ii] ~ bernoulli_logit(phi[ii] * (V[1] - V[2]));
        
        
      }
    }
  }
}


generated quantities{
  
}

