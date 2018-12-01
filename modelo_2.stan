data {
  int<lower=0> N;  // Tamaño de los datos
  int y[N];        // Número de asesinados
  int n[N];        // Offset
  int L;           // Número de estados
  int state[N];    // Estados
  int division[L]; // Divisiones
  int division_no; // Número de division
}

parameters {
  vector[L] beta0; 
  vector[division_no] theta;
  vector<lower = 0>[division_no] theta_sd;
  real phi_param;
  real<lower = 0> lambda;
  real<lower = 0> a;
  real<lower = 0> b;
}

model {
  // Verosimilitud
  for(i in 1:N){
    y[i] ~ binomial(
      n[i],
      inv_logit(beta0[state[i]])
      );
  }
  // Cambio de estado a división
  for(j in 1:L){
    beta0[j] ~ normal(
      theta[division[j]],
      theta_sd[division[j]]
      );
  }
  // Cambio de división  a hiperparámetros
  theta    ~ normal(phi_param, lambda);
  theta_sd ~ gamma(a, b);
  // Priors vagas
  phi_param ~ normal(0, 10);
  lambda    ~ gamma(0.001, 0.001);
  a ~ ;
  b ~ ;
}

generated quantities{
  int yn[N];
  for(i in 1:N){
    yn[i] = binomial_rng(
      n[i],
      inv_logit(beta0[i])
      );
  }
}
