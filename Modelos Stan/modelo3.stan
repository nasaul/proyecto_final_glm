data {
  int<lower=0> N;  // Tamaño de los datos
  int y[N];        // Número de asesinados
  int n[N];        // Offset
  int L;           // Número de estados
  int state[N];    // Estados
  int division[L]; // Divisiones
  int division_no; // Número de division
  int P;           // Número de covariables
  matrix[N,P] X;   // Matriz de covariables
}

parameters {
  vector[L] beta0;
  vector[division_no] theta;
  vector<lower = 0>[division_no] theta_sd;
  real phi_param;
  real<lower = 0> lambda;
  vector[P] beta;
}

transformed parameters {
  vector[N] prob;
  for(i in 1:N){
    prob[i] = exp(beta0[state[i]] + row(X, i) * beta);
  }
}

model {
  // Verosimilitud
  for(i in 1:N){
    y[i]~ poisson(n[i] * prob[i]);
  }
  // Cambio de estado a division
  for(j in 1:L){
    beta0[j] ~ normal(
      theta[division[j]],
      theta_sd[division[j]]
      );
  }
  // Cambio de division a hiperparámetros
  theta    ~ normal(phi_param, lambda);
  // theta_sd ~ gamma(0.001, 0.001);
  // Priors vagas
  phi_param ~ normal(0, 10);
  // lambda    ~ gamma(0.001, 0.001);
  beta      ~ normal(0, 1);
}

generated quantities{
  int yn[N];
  vector[N] log_lik;
  for(i in 1:N){
    yn[i]      = poisson_rng(n[i] * prob[i]);
    log_lik[i] = poisson_lpmf(y[i] | n[i] * prob[i]);
  }
}
