data {
  int<lower=0> N;
  vector[N] imr;
  vector[N] ratio;
  vector[N] y;
}
parameters {
  real alpha;
  real beta1;
  real beta2;
  real<lower=0> sigma;
}

transformed parameters{
  vector[N] mu;
  
  for(i in 1:N){
    mu[i] = alpha + beta1 * imr[i] + beta2*ratio[i];
  }    
}

model {
  //likelihood
  y ~ normal(mu, sigma);

  //priors
  alpha ~ normal(0,1);
  beta1 ~ normal(0,1);
  beta2 ~ normal(0,1);
  sigma ~ normal(0,1);
}
generated quantities {
  vector[N] log_lik;    // pointwise log-likelihood for LOO
  vector[N] y_rep; // replications from posterior predictive dist

  for (i in 1:N) {
    real y_hat_i = alpha + beta1 * imr[i] + beta2*ratio[i];
    log_lik[i] = normal_lpdf(y[i] | y_hat_i, sigma);
    y_rep[i] = normal_rng(y_hat_i, sigma);
  }
  
}