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
}

model {
  // Verosimilitud
  for(i in 1:N){
    y[i] ~ binomial(n[i], (1 - exp(-exp(beta0[state[i]]))));
  }
  // Cambio de estado a división
  for(j in 1:L){
    beta0[j] ~ normal(theta[division[j]],theta_sd[division[j]]);
  }
  // Cambio de división  a hiperparámetros
  theta    ~ normal(phi_param, lambda);
  theta_sd ~ gamma(0.001, .001);
  // Priors vagas
  phi_param ~ normal(0, 10);
  lambda    ~ gamma(0.001, 0.001);
}

generated quantities{
  int yn[N];
  for(i in 1:N){
    yn[i] = binomial_rng(n[i], (1 - exp(-exp(beta0[state[i]]))));
  }
}
