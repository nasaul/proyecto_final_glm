data {
  int<lower=0> N;
  int y[N];
  int n[N];
  int state[N];
  int state_no;
  int K;
  matrix[N,K] X;
}

parameters {
  vector[N] beta0; 
  vector[state_no] theta;
  vector<lower = 0>[state_no] theta_sd;
  vector[K] beta;
}

model {
  for(i in 1:N){
    y[i] ~ binomial(n[i], inv_logit(beta0[i] + dot_product(row(X, i), beta)));
    beta0[i] ~ normal(theta[state[i]], theta_sd[state[i]]);
  }
  beta     ~ normal(0, 10);
  theta    ~ normal(0, 10);
  theta_sd ~ gamma(0.001, 0.001);
}

generated quantities{
  int yn[N];
  for(i in 1:N){
    yn[i] = binomial_rng(n[i], inv_logit(beta0[i] + dot_product(row(X, i), beta)));
  }
}
