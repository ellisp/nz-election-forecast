// ss-simple.stan
// forecast a third election given the results of two earlier ones, and 
// polls by five pollsters over the whole period.  As this will mean
// 2000+ days with n_parties (maybe 7 or so) state to estimate for each day,
// it will have > 10,000 parameters to estimate and takes a long time to run.

data {
  int n_days[2];                   // number of days between first, second and third elections
  int n_parties;                            // number of parties
  real mu_start[n_parties];                 // value at first election
  real mu_finish[n_parties];                // value at second election
  
  // note - 5 pollsters is hard coded in to avoid having to use some kind of ragged array:
  
  int y1_n;                              // number of polls conducted by pollster 1
  real y1_values[y1_n, n_parties];       // actual values in polls for pollster 1
  int y1_days[y1_n];                     // the number of days since first election each poll was taken
  
  int y2_n;
  real y2_values[y2_n, n_parties];       
  int y2_days[y2_n];                     
  
  int y3_n;
  real y3_values[y3_n, n_parties];       
  int y3_days[y3_n];                     
  
  int y4_n;
  real y4_values[y4_n, n_parties];       
  int y4_days[y4_n];                     
  
  int y5_n;
  real y5_values[y5_n, n_parties];       
  int y5_days[y5_n];                     
  
  
}
parameters {
  real<lower=0,upper=1> mu[sum(n_days), n_parties];     // underlying state of vote intention, as a proportion (not percentage)
  real<lower=0> sigma[n_parties];                       // standard deviations for daily innovations for each party
  real d[5, n_parties];                                 // house effects for 5 pollsters and n_parties parties
}

model {
  
  // state model
  sigma ~ normal(0.005, 0.005);
  
  mu[1, ] ~ normal(mu_start, 0.0001); // start very very close to the first election result
  for (i in 2:sum(n_days))
    mu[i, ] ~ normal(mu[i - 1], sigma);
    
  // measurement model
  // 1. Election result for second election
  mu[n_days[1], ] ~ normal(mu_finish, sqrt(.3 * .7 / 10 ^ 5));
  
  // 2. Polls
  for(p in 1:5)
    d[p, ] ~ normal(0.0, 0.075); // ie a fairly loose prior for house effects
  
  for(j in 1:n_parties){
    
    
    for(t in 1:y1_n)
        y1_values[t, j] ~ normal(mu[y1_days[t], j] + d[1, j], 
                              sqrt(mu[y1_days[t], j] * (1 - mu[y1_days[t], j]) / 800));
                              
    for(t in 1:y2_n)
        y2_values[t, j] ~ normal(mu[y2_days[t], j] + d[2, j], 
                              sqrt(mu[y2_days[t], j] * (1 - mu[y2_days[t], j]) / 800));
                              
    for(t in 1:y3_n)
        y3_values[t, j] ~ normal(mu[y3_days[t], j] + d[3, j], 
                              sqrt(mu[y3_days[t], j] * (1 - mu[y3_days[t], j]) / 800));
                              
    for(t in 1:y4_n)
        y4_values[t, j] ~ normal(mu[y4_days[t], j] + d[4, j], 
                              sqrt(mu[y4_days[t], j] * (1 - mu[y4_days[t], j]) / 800));
                              
    for(t in 1:y5_n)
        y5_values[t, j] ~ normal(mu[y5_days[t], j] + d[5, j], 
                              sqrt(mu[y5_days[t], j] * (1 - mu[y5_days[t], j]) / 800));
  }
}
