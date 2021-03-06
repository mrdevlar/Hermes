# Shared Frailty Model


source("R/C-Index.R")
library(rethinking)
library(rstan)
library(dplyr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
options(scipen = 9999)


# Scale to vector function
s = function(x) as.vector(scale(x))

# Weibull Hazard
h_t = function(lifetime, alp, gam, lin_pred){
  exp(lin_pred) * ((alp/gam) * (lifetime/gam)^(alp-1))^(-1)
}



#### Model Parameters ####

N = 500
alp = 3
gam = 5


# Inverter-Level Covariates
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
park_N     = c(150 , 150 , 100 , 50 , 50)
park      = rep(1:5, park_N)
park_var  = c(1, 0.1, 0.5, 0.8, 0.3)
park_eff  = c(2, 1.5, 1.7, 1.3, 1.4)
park_Zij  = numeric()
for(i in seq_along(park_N)){
  out = rgamma(park_N[i], 1/park_var[i], 1/park_var[i])
  park_Zij = c(park_Zij, out)
}
park_Zij = park_Zij * park_eff

summary(park_Zij)



# Model Type
type_N = c(125, 25, 0,
           75 , 50, 25,
           0  , 75, 25,
           40 ,  0, 10,
           0  , 30, 20)
type_N = type_N * (N / sum(type_N))
type = rep( rep(1:3,5) , type_N)
type_eff = rep( rep(c(1.6,1.01,1.3), 5) , type_N)
type_var = rep( rep(c(0.1,0.0001,0.3), 5) , type_N)
type_Wij = sapply(type_var, function(x){rgamma(1, 1/type_var, 1/type_var)})
type_Wij = type_Wij * type_eff

summary(type_Wij)


# Park Weather - Count of extreme events at park level
park_weather = c(0, 3, 2, 1, 3)
park_weather_beta = c(0.2, 0.15, 0.5, 0.1, 0.25)
park_weather      = s(rep(park_weather , park_N))
park_weather_beta = rep(park_weather_beta , park_N)
park_weather_beta = sapply(park_weather_beta, function(x) rnorm(1, x, 0.05))


## Final Linear Predictor

# Turn off linear predictors
# lin_pred = 0


### Two different ways to define the frailty.
# Either as a (I) random effect acting on the linear predictor
# Or as a (II) multiplicative effect acting on the linear predictor
# Both are valid, as Z_{ij} = exp(b_ij^t w_{ij})
# If you want to add/remove the effect, make sure you uncomment the correct two lines, not both. 





# (I)
# Without random effect
lin_pred = x_err1 * beta_err1 + x_err2 * beta_err2 + x_repair * beta_repair +
  x_kWh * beta_kWh + park_weather * park_weather_beta

# With random effect, note bias
# lin_pred = x_err1 * beta_err1 + x_err2 * beta_err2 + x_repair * beta_repair +
#   x_kWh * beta_kWh + park_weather * park_weather_beta +
#   log(park_Zij) + log(type_Wij)


# (II)
# No random effect
# r_lifetime = ((gam^alp) * (-log(runif(N)) * exp(-lin_pred)) )^(1/alp)

# With random effect, note bias
r_lifetime = ((gam^alp) * (-log(runif(N)) * exp(-lin_pred) * park_Zij) )^(1/alp)


## Censoring and observed lifetimes
cens_tweak = 0.1
# cens_tweak = 1
cens_time = runif(N, min = 0, max = max(cens_tweak * r_lifetime) )
d_i = as.numeric(r_lifetime < cens_time)
lifetime = apply(cbind(r_lifetime, cens_time), 1, min)
sum(d_i)

# Turn off censoring
# lifetime = r_lifetime
# d_i = rep(1, length(lifetime))


stan_data = list(
  lifetime = cbind(lifetime,d_i),
  N = N,
  id = id,
  park = park,
  park_N = length(park_N),
  park_weather = park_weather,
  x_err1 = x_err1,
  x_err2 = x_err2,
  x_repair = x_repair,
  x_kWh = x_kWh
)





#### Stan Model ####

m_code = "

functions {
  real log_h_t(real lifetime, real alp, real gam);
  real H_t(real lifetime, real alp, real gam);

  real log_h_t(real lifetime, real alp, real gam){
    return log( (alp/gam) ) + (alp - 1) * log( (lifetime / gam) );
  }
  
  real H_t(real lifetime, real alp, real gam){
    return ( (lifetime / gam) )^alp ;
  }
  
  real surv_dens_log(vector cens_lifetime, real alp, real gam, real sig, real lin_pred){
    real lifetime;
    real d_i;
    real sig_i;

    sig_i    <- sig;
    lifetime <- cens_lifetime[1];
    d_i      <- cens_lifetime[2];
  
    return d_i * log(sig^2) + log(tgamma((1/sig^2) + d_i)) - log(tgamma( (1/sig^2) )) -
            ((1/sig^2) + d_i) * log(1 + sig^2 * H_t(lifetime, alp, gam) * exp(lin_pred)) +
            d_i * (lin_pred + log_h_t(lifetime, alp, gam));
  }

}


data {
  int<lower=1>  N;
  int<lower=1>  park[N];
  int<lower=1>  park_N;
  vector[2]     lifetime[N];
  real          x_err1[N];
  real          x_err2[N];
  real          x_repair[N];
  real          x_kWh[N];
  real          park_weather[N];
}


parameters {
  real<lower=0> alp;
  real<lower=0> gam;
  real<lower=0> sig[park_N];
  real          beta_err1;
  real          beta_err2;
  real          beta_repair;
  real          beta_kWh;
  real          beta_weather;
}


model {
  for(i in 1:N){
      lifetime[i] ~ surv_dens(alp, gam, sig[park[i]], beta_err1 * x_err1[i] + beta_err2 * x_err2[i] + 
                  beta_repair * x_repair[i] + beta_kWh * x_kWh[i] + beta_weather * park_weather[i]);
  }
  
  alp ~ lognormal(0, 1.5);
  gam ~ lognormal(0, 1.5);

  for(j in 1:park_N){
    sig[j] ~ lognormal(0, 1.5);
  }

  beta_err1 ~ normal(0, 1);
  beta_err2 ~ normal(0, 10);
  beta_repair ~ normal(0, 1);
  beta_kWh ~ normal(0,10);
  beta_weather ~ normal(0, 1);
}

generated quantities {
  vector[N] log_lik;
  for (i in 1:N){
    log_lik[i] <- surv_dens_log(lifetime[i], alp, gam, sig[park[i]], beta_err1 * x_err1[i] + beta_err2 * x_err2[i] + 
        beta_repair * x_repair[i] + beta_kWh * x_kWh[i] + beta_weather * park_weather[i]);
  }

}

"

## Fit Model
mf2m = stan(model_code = m_code, data = stan_data, 
            cores = 7, chains = 2, iter = 1e4, warmup = 1e3, 
            control = list(adapt_delta=0.99))

## Parameter Output
print(mf2m, pars = c("alp","gam","sig", "beta_err1","beta_err2","beta_repair","beta_kWh","beta_weather") )
plot(mf2m, pars = c("alp","gam","sig", "beta_err1","beta_err2","beta_repair","beta_kWh","beta_weather") )

## Check Traceplot
traceplot(mf2m, pars = c("alp","gam","sig", "beta_err1","beta_err2","beta_repair","beta_kWh","beta_weather") )

## Get WAIC
WAIC(mf2m)
LOO(mf2m)

## Extract Samples
samp = extract(mf2m, pars = c("alp","gam","sig","beta_err1","beta_err2","beta_repair","beta_kWh","beta_weather") )
samp_lik = extract(mf2m, pars = c("log_lik") )




## Posterior Samples
s_mean = lapply(samp, mean)
s_sd   = lapply(samp, sd)
s_n    = 500

s_alp           = rnorm(s_n, mean = s_mean$alp,          sd = s_sd$alp)
s_gam           = rnorm(s_n, mean = s_mean$gam,          sd = s_sd$gam)
s_sig           = rnorm(s_n, mean = s_mean$sig,          sd = s_sd$sig)
s_beta_err1     = rnorm(s_n, mean = s_mean$beta_err1,    sd = s_sd$beta_err1)
s_beta_err2     = rnorm(s_n, mean = s_mean$beta_err2,    sd = s_sd$beta_err2)
s_beta_repair   = rnorm(s_n, mean = s_mean$beta_repair,  sd = s_sd$beta_repair)
s_beta_kWh      = rnorm(s_n, mean = s_mean$beta_kWh,     sd = s_sd$beta_kWh)
s_beta_weather  = rnorm(s_n, mean = s_mean$beta_weather, sd = s_sd$beta_weather)

# Posterior Linear Predictor
pred_lin_pred = x_err1 * mean(s_beta_err1) + x_err2 * mean(s_beta_err2) + x_repair * mean(s_beta_repair) +
  x_kWh * mean(s_beta_kWh) + park_weather * mean(s_beta_weather)

# Posterior Hazard
hazard = h_t(lifetime, mean(s_alp), mean(s_gam), pred_lin_pred)



# Put it all together
df = data.frame(h_t = hazard,
                s_lik = colMeans(samp_lik$log_lik),
                H_t = -pweibull(lifetime, mean(s_alp), mean(s_gam), lower = FALSE, log = TRUE),
                S_t =  pweibull(lifetime, mean(s_alp), mean(s_gam), lower = FALSE),
                d_i = d_i,
                lifetime = lifetime,
                r_lifetime = r_lifetime,
                diff_times = r_lifetime - lifetime)

# In-sample C-Index
C_Index(df)
# Out-of-sample C-Index
C_Index(df, lifetime = "r_lifetime")
C_Index(df, lifetime = "diff_times")



df %>% ggplot(aes(y = H_t, x= lifetime)) + geom_point()
df %>% ggplot(aes(y = S_t, x= lifetime)) + geom_point()
df %>% ggplot(aes(y = h_t, x= lifetime)) + geom_point()


