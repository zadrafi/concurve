// Normal likelihood with mu fixed as data. Optimising over sigma returns
// the profile log-likelihood at mu_fixed (use rstan::optimizing()).
data {
  int<lower=1> N;
  vector[N] y;
  real mu_fixed;
}
parameters {
  real<lower=0> sigma;
}
model {
  target += normal_lpdf(y | mu_fixed, sigma);
}
