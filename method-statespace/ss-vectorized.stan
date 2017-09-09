// ss-vectorized.stan
// forecast a third election given the results of two earlier ones, and 
// polls by five pollsters over the whole period.  As this will mean
// 2000+ days with n_parties (maybe 7 or so) state to estimate for each day,
// it will have > 10,000 parameters to estimate and takes a long time to run.

// an alternative approach to the below would be to specify mu to be an array of simplex vectors,
// constrained to add up to 1.  The rationale for *not* doing this is that it isn't really clear
// what the proportions in mu really are of - because of population change, and turnout to vote.
// So it's not certain that forcing them to add up to exactly 1 will help much, and it will certainly
// add to complexity.

data {
  int n_days[2];                   // number of days between first, second and third elections
  int n_parties;                            // number of parties
  real mu_start[n_parties];                 // value at first election
  real mu_finish[n_parties];                // value at second election
  real inflator;                      // amount by which to multiply the standard error of polls
  
  // note - pollsters are individually hard coded in to avoid having to use some kind of ragged array:
  int n_pollsters;
  
  int y1_n;                              // number of polls conducted by pollster 1
  real y1_values[y1_n, n_parties];       // actual values in polls for pollster 1
  int y1_days[y1_n];                     // the number of days since first election each poll was taken
  real y1_se[n_parties];                 // the standard error for each party from pollster 1 (note this stays the same throughout history)
  
  int y2_n;
  real y2_values[y2_n, n_parties];       
  int y2_days[y2_n];                     
  real y2_se[n_parties];
  
  int y3_n;
  real y3_values[y3_n, n_parties];       
  int y3_days[y3_n];                     
  real y3_se[n_parties];
  
  int y4_n;
  real y4_values[y4_n, n_parties];       
  int y4_days[y4_n];                     
  real y4_se[n_parties];
  
  int y5_n;
  real y5_values[y5_n, n_parties];       
  int y5_days[y5_n];                     
  real y5_se[n_parties];
  
  int y6_n;
  real y6_values[y6_n, n_parties];       
  int y6_days[y6_n];                     
  real y6_se[n_parties];
  
  
}
parameters {
  vector[n_parties] epsilon[sum(n_days)];     // innovations in underlying state of vote intention
  corr_matrix[n_parties] omega;
  real<lower=0> sigma[n_parties];           // standard deviations for daily innovations for each party
  real d[n_pollsters, n_parties];                     // house effects for n_pollsters and n_parties combinations
}

transformed parameters {
  real mu[sum(n_days), n_parties];     // underlying state of vote intention, as a proportion (not percentage)
  
  mu[1, ] = mu_start;
  for(i in 2:sum(n_days)){
    for(j in 1:n_parties){
      mu[i, j] = mu[i-1, j] + epsilon[i][j] * sigma[j];
    }
  }
  
}

model {
  vector[n_parties] zeroes;
  zeroes = rep_vector(0, n_parties);
  
  // prior for scaling of innovations
  sigma ~ normal(0.002, 0.001);
  
  // prior for correlation matrix of innovations, on standardised scale (so SD = 1)
  omega ~ lkj_corr(1); // LKJ prior on the correlation matrix 
  
  // prior for incumbency, from separate "political science" style model.  Doesn't work...
  // Fixing this could be key to making this a decent combination of political science and polls.
  // mu[5, sum(n_days)] ~ normal(0.45, 0.06);
  
  // innovations in the state space, on standardised scale
  // Note - when this is done as iid normal rather than multi_normal it makes things
  // dramatically faster (20 minutes versus 80 minutes), but at cost to modelling coherence.
  // We need this multivariate approach to adequately capture the correlation
  epsilon ~ multi_normal(zeroes, omega);  
  
  // measurement model
  // 1. Election result for second election - treat as observation with a very big sample size
  mu_finish ~ normal(mu[n_days[1], ], sqrt(.3 * .7 / 10 ^ 5));
  
  // 2. Polls
  
  for(p in 1:n_pollsters)
    d[p, ] ~ normal(0.0, 0.025); // ie a fairly loose prior for the house effects for each pollster.  
    // Brought down from 0.075 to 0.025 on 18 August 2017 because it was converging to non-sensible results.
  
  // This can probably be improved by vectorising too  
  for(j in 1:n_parties){
    
    for(t in 1:y1_n)
        y1_values[t, j] ~ normal(mu[y1_days[t], j] + d[1, j], y1_se[j] * inflator);
                              
    for(t in 1:y2_n)
        y2_values[t, j] ~ normal(mu[y2_days[t], j] + d[2, j], y2_se[j] * inflator);
                              
    for(t in 1:y3_n)
        y3_values[t, j] ~ normal(mu[y3_days[t], j] + d[3, j], y3_se[j] * inflator);
                              
    for(t in 1:y4_n)
        y4_values[t, j] ~ normal(mu[y4_days[t], j] + d[4, j], y4_se[j] * inflator);
                              
    for(t in 1:y5_n)
        y5_values[t, j] ~ normal(mu[y5_days[t], j] + d[5, j], y5_se[j] * inflator);
        
    for(t in 1:y6_n)
        y6_values[t, j] ~ normal(mu[y6_days[t], j] + d[6, j], y6_se[j] * inflator);
        
  }
}
