data {
  int <lower=0> n; // number of observations
  int <lower=0> d; // number of predictors
  vector [n] y; // outputs
  matrix [n,d] x; // inputs
  
  //Horseshoe parameters
  real <lower=1> nu_local; // [JUST SET TO 1 FOR CAUCHY] degrees of freedom for the half-t priors for lambdas
  real <lower=0> scale_global; // [SET TO tau0 BASED ON EXPECTED NONZEROS] scale for the half-t prior for tau
  real <lower=1> nu_global; // [JUST SET TO 1 FOR CAUCHY] degrees of freedom for the half-t priors for tau
  real <lower=0> slab_df; // UNUSED for unregularized horseshoe
  real <lower=0> slab_scale; // UNUSED for unregularized horseshoe
}
parameters {
  real logsigma;
  vector [d] z;
  real <lower=0> tau; // global shrinkage parameter
  vector <lower=0>[d] lambda; // local shrinkage parameter
}
transformed parameters {
  real <lower=0> sigma; // noise std
  vector [d] beta; // regression coefficients
  vector [n] f; // latent function values
  sigma = exp(logsigma);
  beta = (z).*lambda*tau;
  f = x*beta;
}
model {
  // half -t priors for lambdas and tau
  z ~ normal (0, 1);
  lambda ~ student_t(nu_local, 0, 1);
  tau ~ student_t(nu_global, 0, scale_global * sigma );
  y ~ normal(f, sigma);
}
