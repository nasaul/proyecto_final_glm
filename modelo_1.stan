data {
  int<lower=0> N;
  int y[N];
  int n[N];
  int division[N];
  int division_no;
}

parameters {
  vector[division_no] theta;
  real phi_param;
  real<lower = 0> lambda;
}

model {
  for(i in 1:N){
    y[i] ~ binomial(n[i], inv_logit(theta[division[i]]));
  }
  theta     ~ normal(phi_param, lambda);
  phi_param ~ normal(0, 10);
  lambda    ~ gamma(0.001, 0.001);
}

generated quantities{
  int yn[N];
  for(i in 1:N){
    yn[i]  = binomial_rng(
      n[i],
      inv_logit(theta[division[i]])
      );
  }
}
