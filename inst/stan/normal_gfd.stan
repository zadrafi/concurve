// Generalized fiducial distribution for the normal location-scale model.
//
// Data-generating equation:  y_i = mu + sigma * z_i,  z_i ~ N(0, 1).
// Hannig's Jacobian formula gives the GFD density
//
//     r(theta | y)  proportional to  L(y, theta) * J(y, theta),
//     J = det( grad_theta F' grad_theta F )^(1/2).
//
// Here grad_theta F has rows (1, u_i) with u_i = (y_i - mu) / sigma, so
//
//     det(.) = n * sum(u^2) - (sum u)^2 = n (n - 1) s^2 / sigma^2
//     J      = sqrt(n (n - 1)) * s / sigma   proportional to  1 / sigma.
//
// The Jacobian is derived for this model, not assumed; other models need
// their own. The marginal GFD of mu is exactly t_{n-1}(ybar, s / sqrt(n)).
data {
  int<lower=1> N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  target += normal_lpdf(y | mu, sigma);  // likelihood
  target += -log(sigma);                 // fiducial Jacobian, up to a constant
}
