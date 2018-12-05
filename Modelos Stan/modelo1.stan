data {
  int<lower=0> N;  // Tamaño de los datos
  int y[N];        // Número de asesinados
  int n[N];        // Offset
  int division[N]; // Divisiones
  int division_no; // Número de division
  int liga;
  int distribucion;
}

parameters {
  vector[division_no] theta;
  real phi_param;
  real<lower = 0> lambda;
}

transformed parameters {
  vector[N] prob;
  for(i in 1:N){
    if(liga == 1){
      prob[i] = inv_logit(theta[division[i]]);
    } else if(liga == 2){
      prob[i] = Phi(theta[division[i]]);
    } else if(liga == 3){
      prob[i] = 1 - exp(-exp(theta[division[i]]));
    } else if(liga == 4){
      prob[i] = exp(-exp(theta[division[i]]));
    } else if(liga == 5){
      prob[i] = exp(theta[division[i]]);
    }
  }
}

model {
  if(distribucion == 1){
      y ~ binomial(n, prob);
  } else {
    for(i in 1:N){
      y[i] ~ poisson(n[i] * prob[i]);
    }
  }
  theta     ~ normal(phi_param, lambda);
  phi_param ~ normal(0, 10);
  lambda    ~ gamma(0.001, 0.001);
}

generated quantities{
  int yn[N];
  vector[N] log_lik;

  if(distribucion == 1){
    yn = binomial_rng(n, prob);
    for(i in 1:N){
      log_lik[i] = binomial_lpmf(y[i] | n[i], prob[i]);
    }
  } else {
    for(i in 1:N){
      yn[i] = poisson_rng(n[i] * prob[i]);
      log_lik[i] = poisson_lpmf(y[i] | n[i] * prob[i]);
    }
  }

}
