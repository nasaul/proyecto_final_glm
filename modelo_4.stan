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
  // Interceptos
  vector[N] beta0; // Condado
  vector[state_no] theta0; // Estado
  vector<lower = 0>[state_no] theta0_sd; // Estado
  // Variables explicativas
  matrix[N, K] beta; // Condado
  vector[state_no] theta; // Estado
  vector<lower = 0>[state_no] theta_sd; // Estado
}

model {
  for(i in 1:N){
    y[i]     ~ binomial(
      n[i],
      inv_logit(
        beta0[i] +
        dot_product(row(X, i), row(beta, i))
        )
      );
    beta0[i] ~ normal(
      theta0[state[i]],
      theta0_sd[state[i]]
      );
    
  }
  beta0    ~ normal(0, 10);
  theta0   ~ normal(0, 10);
  theta_sd ~ gamma(0.001, 0.001);
}

generated quantities{
  int yn[N];
  for(i in 1:N){
    yn[i] = binomial_rng(n[i], inv_logit(beta0[i] + dot_product(row(X, i), beta)));
  }
}
