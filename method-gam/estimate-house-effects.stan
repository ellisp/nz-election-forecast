data {
  int N;                        // sample size - number of elections
  real disc[N];                // historical errors / discrepencies
}
parameters {
  real house_effect;
  real<lower=0> sigma;
}
model {
  sigma ~ cauchy(0, 5);
  house_effect ~ normal(0, 0.15);
  for (i in 1:N) 
      disc[i] ~ normal(house_effect, sigma);
}
