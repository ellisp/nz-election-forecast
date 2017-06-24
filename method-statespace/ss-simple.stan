// ss-simple.stan

data {
  int<lower=1> n_days[2];           
  int n_parties;
  real mu_start[n_parties];                 // value at starting election
  real mu_finish[n_parties];                // value at final election
}
parameters {
  real<lower=0,upper=100> mu[sum(n_days), n_parties];               // underlying state of vote intention
  real<lower=0> sigma[n_parties];
}

model {
  
  // state model
  sigma ~ normal(0.005, 0.005);
  
  mu[1, ] ~ normal(mu_start, 0.0001);
  for (i in 2:sum(n_days))
    mu[i, ] ~ normal(mu[i - 1], sigma);
    
  // measurement model
  // 1. Election result
  mu[n_days[1], ] ~ normal(mu_finish, 0.0001);
  
}
