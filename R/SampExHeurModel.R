### Sample Extrapolation Heuristic Model


get_samples <- function(samp, N_samp){
  
  if (length(samp) > N_samp){
    new_samp <- samp[length(samp) - N_samp : length(samp)]
  } else if (length(samp) > 0){
    new_samp <- sample(samp, N_samp, replace = TRUE)
  } else {
    new_samp <- round(rnorm(N_samp, 0, 1))
  }
  
}

SampEx <- function(N, samples_A, samples_B, max_T, t, G, Yt){
  
  # N: the memory capacity. This number of observations is considered
  # samples_A / samples_B: samples drawn so far from each option
  # max_T: maximum trial number
  # t: current trial number
  # G: goal
  # Yt: number of points earned by trial t
  
  # get rid of NAs
  samples_A <- samples_A[!is.na(samples_A)]
  samples_B <- samples_B[!is.na(samples_B)]
  
  # get Recent Distribution
  RD_A <- get_samples(samples_A, N)
  RD_B <- get_samples(samples_B, N)
  
  # number of remaining trials
  R <- max_T - t
  
  # create Recent Extrapolated Distribution
  ReD_A <- RD_A * R
  ReD_B <- RD_B * R
  
  # for each ReD value, check whether goal is reached or not (ReD binary)
  ReD_bin_A <- ReD_A > (G - Yt)
  ReD_bin_B <- ReD_B > (G - Yt)
  
  # calculate likelyhood of reaching the goal from ReD
  pi_A <- mean(ReD_bin_A)
  pi_B <- mean(ReD_bin_B)
  
  pis <- list(pi_A, pi_B)
  
  
}

SampExFit <- function(Q, chd, v_As, v_Bs, T, curr_t, goal, curr_Y, pred = FALSE){
  
  N_ret <- round(Q[1])
  phi_s <- Q[2]

  pi_As <- vector("numeric", length = length(chd))
  pi_Bs <- vector("numeric", length = length(chd))

  for (jj in seq_along(chd)){

  	ind <- (floor(jj / 25) * 25 + 1) : jj

  	ps <- SampEx(N_ret, v_As[ind], v_Bs[ind], T, curr_t[jj], goal, curr_Y[jj])

  	pi_As[jj] <- ps[[1]]
  	pi_Bs[jj] <- ps[[2]]

  }

  eee <- rep(NA, length(pi_As))

  eee <- chru(pi_As, pi_Bs, phi_s)

  eee[eee > .99999] <- .99999
  eee[eee < .00001] <- .00001

  if(pred==TRUE) return((round(eee) == chd))

  Gsq <- 2*sum(chd[chd!=0]*(log(chd[chd!=0])-log(eee[chd!=0])))
  
  return(Gsq)

}


# Logistic Choice Rule
chru <- function(a,b,phi){
  1/(1 + exp(-phi*(a - b)))
} 




