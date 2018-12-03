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
  vector[division_no] theta0;
  vector<lower = 0>[division_no] theta0_sd;
  real phi_param;
  real<lower = 0> lambda;
  matrix[L, P] beta;
  matrix[division_no, P] theta;
  matrix<lower = 0>[division_no, P] theta_sd;
  vector[P] cov_hiper;
  vector[P] cov_sd_hiper;
}

model {
  // Verosimilitud 
  for(i in 1:N){
    y[i]~ binomial(
      n[i],
      1 - exp(-exp(beta0[state[i]] + dot_product(row(X, i), row(beta, state[i]))))
      );
  }
  // Cambio de estado a division
  for(j in 1:L){
    beta0[j] ~ normal(
      theta0[division[j]],
      theta0_sd[division[j]]
      );
      for(p in 1:P){
        beta[j, p] ~ normal(
          theta[division_no, p],
          theta_sd[division_no, p]
        );
      }
  }
  // Cambio de division a hiperparámetros
  theta0    ~ normal(phi_param, lambda);
  for(p in 1:P){
    theta[, p] ~ normal(cov_hiper[p], cov_sd_hiper);
  }
  // Priors vagas
  phi_param ~ normal(0, 10);
  cov_hiper ~ normal(0, 10);

}

generated quantities{
  int yn[N];
  for(i in 1:N){
    yn[i] = binomial_rng(
      n[i],
      1 - exp(-exp(beta0[state[i]] + dot_product(row(X, i), row(beta, state[i]))))
      )
      ;
  }
}
