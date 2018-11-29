data {
  int<lower=0> N;
  int y[N];
  int n[N];
  int state[N];
  int state_no;
}

parameters {
  vector[state_no] theta;
}

model {
  for(i in 1:N){
    y[i] ~ binomial(n[i], inv_logit(theta[state[i]]));
  }
  for(j in 1:state_no){
    theta[j] ~ normal(0,100);
  }
}

generated quantities{
  int yn[N];
  for(i in 1:N){
    yn[i]  = binomial_rng(n[i], inv_logit(theta[state[i]]));
  }
}
