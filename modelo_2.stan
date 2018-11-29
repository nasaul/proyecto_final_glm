data {
  int<lower=0> N;
  int y[N];
  int n[N];
  int state[N];
  int state_no;
}

parameters {
  vector[N] beta; 
  vector[state_no] theta;
  vector<lower = 0>[state_no] theta_sd;
}

model {
  for(i in 1:N){
    y[i] ~ binomial(n[i], inv_logit(beta[i]));
    beta[i] ~ normal(theta[state[i]], theta_sd[state[i]]);
  }
  
  theta    ~ normal(0, 100);
  theta_sd ~ gamma(0.001, 0.001);
}

generated quantities{
  int yn[N];
  for(i in 1:N){
    yn[i] = binomial_rng(n[i], inv_logit(beta[i]));
  }
}
