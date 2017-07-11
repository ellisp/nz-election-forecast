// ss-vectorized.stan
// forecast a third election given the results of two earlier ones, and 
// polls by five pollsters over the whole period.  As this will mean
// 2000+ days with n_parties (maybe 7 or so) state to estimate for each day,
// it will have > 10,000 parameters to estimate and takes a long time to run.

data {
  int n_days[2];                   // number of days between first, second and third elections
  int n_parties;                            // number of parties
  real mu_start[n_parties];                 // value at first election
  real mu_finish[n_parties];                // value at second election
  real inflator;                      // amount by which to multiply the standard error of polls
  
  // note - 5 pollsters is hard coded in to avoid having to use some kind of ragged array:
  
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
  
}
parameters {
  real epsilon[sum(n_days), n_parties];     // innovations in underlying state of vote intention
  real<lower=0> sigma[n_parties];           // standard deviations for daily innovations for each party
  real d[5, n_parties];                     // house effects for 5 pollsters and n_parties parties
}

transformed parameters {
  real mu[sum(n_days), n_parties];     // underlying state of vote intention, as a proportion (not percentage)
  mu[1, ] = mu_start;
  for(i in 2:sum(n_days)){
    for(j in 1:n_parties){
      mu[i, j] = mu[i-1, j] + epsilon[i, j] * sigma[j];
    }
    
  }
}

model {
  
  // prior for standard deviation of innovations
  sigma ~ normal(0.002, 0.001);
  
  // innovations in the state space 
  // (can probably be improved by vectorising epsilon)
  for(j in 1:n_parties){
      epsilon[ , j] ~ normal(0, 1);  
    }
  
    
    
  // measurement model
  // 1. Election result for second election - treat as observation with a very big sample size
  mu_finish ~ normal(mu[n_days[1], ], sqrt(.3 * .7 / 10 ^ 5));
  
  // 2. Polls
  
  for(p in 1:5)
    d[p, ] ~ normal(0.0, 0.075); // ie a fairly loose prior for the five house effects
  
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
  }
}
