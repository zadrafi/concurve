// Unrestricted normal likelihood; optimising gives the joint MLE and the
// maximum of the profile log-likelihood.
data {
  int<lower=1> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  target += normal_lpdf(y | mu, sigma);
}
