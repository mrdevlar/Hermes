q_mhm = function(q, alp, gam, lin_pred){
  ((gam^alp) * (-log(q) * exp(-lin_pred)) )^(1/alp)
}
h_t = function(lifetime, alp, gam, lin_pred){
  exp(lin_pred) * (alp/gam) * (lifetime/gam)^(alp-1)
}

Csurv = function(t, t0, alp, gam, lin_pred){
  exp((alp * exp(lin_pred) * (t-t0))/gam)
}

surv = function(t, alp, gam, lin_pred){
  exp(-(t * alp * exp(lin_pred)) /gam)
}

summary(rweibull(1e5, alp, gam))

1 - pweibull(159, alp, gam)


x = lifetime[3]
y = lin_pred[3]

z = 1 - pweibull(x, alp, gam)

S_t0 = z^exp(y)

t_new = 500
(1 - pweibull(t_new, alp, gam))^exp(y) / S_t0

qweibull(0.5, alp, gam) / S_t0

q_mhm(0.5, alp, gam, y) / S_t0

survs = numeric()
csurv = numeric()
chazs = numeric()
csurv2 = numeric()
for(i in seq_along(lifetime)){
  survs = c(survs, (1 - pweibull(lifetime[i], alp, gam))^exp(lin_pred[i]) )
  csurv = c(csurv, (1 - pweibull(lifetime[i]+1, alp, gam))^exp(lin_pred[i])  / (1 - pweibull(lifetime[i], alp, gam))^exp(lin_pred[i]))
  chazs  = c(chazs, h_t(lifetime[i], alp, gam, lin_pred[i]))
  csurv2 = c(csurv2, Csurv(lifetime[i]+1, lifetime[i], alp, gam, lifetime[i]))
}

x = data.frame(csurv,chazs,survs, d_i)




csurv3 = function(t0, t, alp, gam, lin_pred){
  (1 - pweibull(t, alp, gam))^exp(lin_pred)  / (1 - pweibull(t0, alp, gam))^exp(lin_pred)
}

sapply(1:length(lifetime), function(i){
  csurv3(lifetime[i], lifetime[i]+14, alp, gam, lin_pred[i])
})


exp(csurv3(lifetime[1], lifetime[1]+14, alp, gam, lin_pred[1]))
csurv3(lifetime[1], lifetime[1]+14, alp, gam, lin_pred[1])




lsurv1(lifetime[1], lifetime[1]+1, alp, gam, lin_pred[1])
lsurv2(lifetime[1], lifetime[1]+1, alp, gam, lin_pred[1])




S_t = function(t, alp, gam, lin_pred){
  (1 - pweibull(t, alp, gam))^exp(lin_pred)
}






## Order Alive Inverters by Hazard, Get Top 20
df %>%
  filter(d_i == 0) %>%
  arrange(s_t) %>%
  top_n(20)

df %>%
  filter(d_i == 0) %>%
  ggplot(aes(y = s_t, x = lifetime)) + 
  geom_point(alpha = 0.2) 


df %>%
  filter(d_i == 0) %>%
  arrange(desc(s_t)) %>%
  ggplot(aes(y = h_t, x = sc_t)) + geom_point(alpha=0.2)



y = S_t(seq(0,600,1), mean(s_alp), mean(s_gam), pred_lin_pred[2]) / S_t(170, mean(s_alp), mean(s_gam), pred_lin_pred[2])
plot(seq(0,600,0.1), y, type="l")

(1 - pweibull(1:600, alp, gam))

mw = function(alp, gam){
  gam * (log(2))^(1/alp)
}


dfp = data.frame(pred_lin_pred, lin_pred)
dfp %>%
  ggplot(aes(x = pred_lin_pred, y = lin_pred)) + geom_point()



df %>%
  filter(d_i == 1) %>%
  ggplot(aes(x = r_lifetime, y = h_t)) + geom_point()


h_t = function(lifetime, alp, gam, lin_pred){
  exp(-lin_pred) * (alp/gam) * (lifetime/gam)^(alp-1)
}

H_t = function(lifetime, alp, gam, lin_pred){
  exp(lin_pred) * ( (lifetime / gam) )^alp  
}
 


