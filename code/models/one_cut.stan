data {
  int<lower=1> N;      
  vector[N] x;
  vector[N] y;
  //vector[2] cutpoiNt;
}

parameters {
  real alpha;
  //vector[2] beta;
  real beta1;
  real beta2;
  real<lower=0> sigma;
  real<lower=0, upper=0.1> cutpoint1;
  //real<lower=0.06, upper=0.1> cutpoint2;
} 

transformed parameters{
    vector[N] x2; // indicator variable for whether x_i > cutpoint[1]
    //vector[N] x3; // indicator variable for whether x_i > cutpoint[2]
    vector[N] mu;
  
  for (i in 1:N) {
    if (x[i] < cutpoint1) {
      x2[i] = 0;
      //x3[i] = 0;
    } 
    //else if (cutpoint1 <= x[i] < cutpoint2){
    //  x2[i] = 1;
      //x3[i] = 0;
    //}
    else{
      x2[i] = 1;
      //x3[i] = 1;
    }
  }

  for(i in 1:N){
    mu[i] = alpha +  beta1* x[i] + beta2 * (x[i] - cutpoint1) * x2[i]; //+  beta[3] * (x[i] - cutpoint2) * x3[i];
  }
}

model {
  
  alpha ~ normal(0, 1);
  beta1 ~ normal(0, 10);
  beta2 ~ normal(0, 10);
  sigma ~ normal(0, 1);
  cutpoint1 ~ normal(0, 0.1);
  // cutpoint2 ~ normal(0.06, 0.001);
  
  y ~ normal(mu, sigma);
}

generated quantities{
  vector[N] log_lik;    // pointwise log-likelihood for LOO
  vector[N] y_rep; // replications from posterior predictive dist
  
  for (i in 1:N) {
    real y_hat_i = alpha + beta1 * x[i] + beta2 * (x[i] - cutpoint1) * x2[i];
    log_lik[i] = normal_lpdf(y[i] | y_hat_i, sigma);
    y_rep[i] = normal_rng(y_hat_i, sigma);
  }
}
