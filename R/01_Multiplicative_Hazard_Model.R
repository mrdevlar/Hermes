# Multiplicative Hazard Model

library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(scipen = 9999)

set.seed(417674)

# Scale to vector function
s = function(x) as.vector(scale(x))

#### Model Parameters ####

N = 500
alp = 1.5
gam = 3



# id	type	weather_l30d
beta_err1   = 0.1 # err_code01_l30d
beta_err2   = 3  # err_code02_l30d - Severe
beta_repair = -0.15 # days since repair
beta_kWh    = -4 #kwh 

x_err1   = s(rpois(N, 5)) # err_code01_l30d
x_err2   = s(rbinom(N, 1, 0.1)) # err_code02_l30d - Severe
x_repair = s(rpois(N, 20)) # repair_days

x_kWh    = rnorm(N, 0, 0.01) # daily output
x_kWh[sample(N, N*0.2)] = -abs(rnorm(N*0.2, 0, 0.1)) # extreme output prior to failure (only in 20% of cases)
x_kWh    = s(x_kWh)

id = 1:N

# Park Effect
parkN     = c(150 , 150 , 100 , 50 , 50)
park      = rep(1:5, parkN)
park_eff  = c(1, 0.1, 0.5, 0.8, 0.3)
park_Zij  = numeric()
for(i in seq_along(parkN)){
    out = rgamma(parkN[i], 1/park_eff[i], 1/park_eff[i])
    park_Zij = c(park_Zij, out)
}
summary(park_Zij)

# park_Zij = 2
# park_Zij = rgamma(N, 1/0.1, 1/0.1)


# Model Type
type_size = c(125, 25, 0,
              75 , 50, 25,
              0  , 75, 25,
              40 ,  0, 10,
              0  , 30, 20)
type_size = type_size * (N / sum(type_size))
type = rep( rep(1:3,5) , type_size)
type_eff = rep( rep(c(0.6,0.01,0.3), 5) , type_size)
type_var = rep( rep(c(0.1,0.0001,0.3), 5) , type_size)
type_Wij = sapply(type_var, function(x){rgamma(1, 1/type_var, 1/type_var)})
# type_Wij = type_Wij * type_eff


# Park Weather





## Final Linear Predictor

### Two different ways to define the frailty.
# Either as a (I) random effect acting on the linear predictor
# Or as a (II) multiplicative effect acting on the linear predictor
# Both are valid, as Z_{ij} = exp(b_ij^t w_{ij})
# If you want to add/remove the effect, make sure you uncomment the correct two lines, not both. 



# (I)
# Without random effect
lin_pred = x_err1 * beta_err1 + x_err2 * beta_err2 + x_repair * beta_repair + x_kWh * beta_kWh

# With random effect, note bias
# lin_pred = x_err1 * beta_err1 + x_err2 * beta_err2 + x_repair * beta_repair + x_kWh * beta_kWh + log(park_Zij) + log(type_Wij)

# (II)
# No random effect
lifetime = ((gam^alp) * (-log(runif(N)) / exp(lin_pred)) )^(1/alp)

# With random effect, note bias
# lifetime = ((gam^alp) * (-log(runif(N)) * exp(-lin_pred) * park_Zij * type_Wij) )^(1/alp)


## Censoring and observed lifetimes
cens_time = runif(N, min = 0, max = max(lifetime))
d_i = as.numeric(lifetime < cens_time)
lifetime = apply(cbind(lifetime, cens_time), 1, min)



stan_data = list(
  lifetime = cbind(lifetime,d_i),
  N = N,
  id = id,
  park = park,
  x_err1 = x_err1,
  x_err2 = x_err2,
  x_repair = x_repair,
  x_kWh = x_kWh
)




#### Stan Model ####

m_code = "

functions {
  real log_h_t(real lifetime, real alp, real gam, real lin_pred);
  real H_t(real lifetime, real alp, real gam, real lin_pred);

  real log_h_t(real lifetime, real alp, real gam, real lin_pred){
    return log( (alp/gam) ) + (alp - 1) * log( (lifetime / gam) ) + lin_pred;
}

  real H_t(real lifetime, real alp, real gam, real lin_pred){
    return exp(lin_pred) * ( (lifetime / gam) )^alp ;
}

  real surv_dens_log(vector cens_lifetime, real alp, real gam, real lin_pred){
    real lifetime;
    real d_i;
  
    lifetime <- cens_lifetime[1];
    d_i      <- cens_lifetime[2];
    
    return d_i * log_h_t(lifetime, alp, gam, lin_pred) - H_t(lifetime, alp, gam, lin_pred);
  }
}


data {
  int<lower=0>  N;
  vector[2]     lifetime[N];
  real          x_err1[N];
  real          x_err2[N];
  real          x_repair[N];
  real          x_kWh[N];
}


parameters {
  real<lower=0> alp;
  real<lower=0> gam;
  real          beta_err1;
  real          beta_err2;
  real          beta_repair;
  real          beta_kWh;
}


model {
  for(i in 1:N){
    lifetime[i] ~ surv_dens(alp, gam, beta_err1 * x_err1[i] + beta_err2 * x_err2[i] + beta_repair * x_repair[i] + beta_kWh * x_kWh[i]);
  }

  alp ~ lognormal(0, 1.5);
  gam ~ lognormal(0, 1.5);
  
  beta_err1 ~ normal(0, 1);
  beta_err2 ~ normal(0, 10);
  beta_repair ~ normal(0, 1);
  beta_kWh ~ normal(0,10);

}

"

mhazm = stan(model_code = m_code, data = stan_data, cores = 7, chains = 2, iter = 1e4, warmup = 1e3)

print(mhazm)

