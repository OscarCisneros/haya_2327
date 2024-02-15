#Ajuste del modelo de incremento diametral individual
#Modelo de Richards, según el trabajo de Sharma et al. 2019

library(tidyverse)
library(minpack.lm)
#library(patchwork)

m.Richards <- nlsLM(
  inc_an ~ (a1*Ho_ifn3^a2 + a3*ab_ifn3 + a4*(Dn_ifn3/dgm_ifn3)) * 
    ((1-exp(-b2*Dn_ifn3))^(b3-1))*exp(-b2*Dn_ifn3),
  data = dat_inc,
  start = list(a1 = -0.23486, a2 = 0.929377, a3 = -0.0773, a4 = 18.82619, b2 = 0.023836, b3 = 3.468139)
)


m.Richards <- nlsLM(
  inc_diam  ~ (a1*Ho_ifn3^a2 + a3*ab_ifn3 + a4*(Dn_ifn3/dgm_ifn3)) * 
    ((1-exp(-b2*Dn_ifn3))^(b3-1))*exp(-b2*Dn_ifn3),
  data = dat_inc,
  start = list(a1 = 2.026, a2 = 0.092, a3 = -0.029, a4 = 0.280, b2 = 0.0002835, b3 = 1.4447351),
  control = nls.lm.control(maxiter = 200)
)
summary(m.Richards)



m.Gompertz <- nlsLM(
  inc_an  ~ (a1*Ho_ifn3^a2 + a3*ab_ifn3 + a4*(Dn_ifn3/dgm_ifn3)) * 
    (exp(-b2*exp(-b3*Dn_ifn3)-b3*Dn_ifn3)),
  data = dat_inc %>% filter(inc_an <= quantile(dat_inc$inc_an, 0.95)[[1]]),
  start = list(a1 = 2.026, a2 = 0.092, a3 = -0.029, a4 = 0.280, b2 = 0.0002835, b3 = 1.4447351),
  control = nls.lm.control(maxiter = 200)
)
summary(m.Gompertz)
uu <- predict(m.Gompertz, newdata = dat_inc %>% filter(inc_an <= quantile(dat_inc$inc_an, 0.95)[[1]]))
evalua <- dat_inc %>%
  filter(inc_an <= quantile(dat_inc$inc_an, 0.95)[[1]]) %>%
  mutate(predicho = uu)

ggplot(evalua, aes(x= inc_an, y = predicho))+geom_point()

############## inc an
m.Richards <- nlsLM(
  inc_an  ~ (a1*Ho_ifn3^a2 + a3*ab_ifn3 + a4*(Dn_ifn3/dgm_ifn3)) * 
    ((1-exp(-b2*Dn_ifn3))^(b3-1))*exp(-b2*Dn_ifn3),
  data = dat_inc %>% filter(inc_an <= quantile(dat_inc$inc_an, 0.90)[[1]] & inc_an >=0.2),
  start = list(a1 = 2.026, a2 = 0.092, a3 = -0.029, a4 = 0.280, b2 = 0.0002835, b3 = 1.4447351),
  control = nls.lm.control(maxiter = 200)
)
summary(m.Richards)
uu <- predict(m.Richards, newdata = dat_inc %>% filter(inc_an <= quantile(dat_inc$inc_an, 0.90)[[1]]  & inc_an >=0.2))
evalua <- dat_inc %>%
  filter(inc_an <= quantile(dat_inc$inc_an, 0.90)[[1]] & inc_an >=0.2) %>%
  mutate(predicho = uu) %>%
  mutate(residuos = inc_an - predicho)

rmse = sqrt(mean(evalua$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(evalua$inc_an)

ggplot(evalua, aes(x= inc_an, y = predicho))+geom_point()

func_outl <- function(x) {
  lim_sup = quantile(x, 0.75)[[1]]+1.5*IQR(x)
  lim_inf = quantile(x, 0.25)[[1]]-1.5*IQR(x)
  outl = x < lim_inf | x > lim_sup
}

bb <- dat_inc %>%
  select(Dn, Dn_ifn3, Ho_ifn3, ab_ifn3, dgm_ifn3, inc_an) %>%
  mutate(outl_inc = func_outl(inc_an))
#diagnose_numeric(bb)

############## inc_an sin ouliers
m.Richards <- nlsLM(
  inc_an  ~ (a1*Ho_ifn3^a2 + a3*ab_ifn3 + a4*(Dn_ifn3/dgm_ifn3)) * 
    ((1-exp(-b2*Dn_ifn3))^(b3-1))*exp(-b2*Dn_ifn3),
  data = bb %>% filter(!outl_inc),
  start = list(a1 = 2.026, a2 = 0.092, a3 = -0.029, a4 = 0.280, b2 = 0.0002835, b3 = 1.4447351),
  control = nls.lm.control(maxiter = 200)
)
summary(m.Richards)
save(m.Richards, file = "datos/mod_inc_diam_individual")
uu <- predict(m.Richards, newdata = dat_inc)
evalua <- dat_inc %>%
  mutate(predicho = uu) %>%
  mutate(predicho_Dn = Dn_ifn3+predicho*anno) %>%
  mutate(residuo_Dn = Dn -predicho_Dn)

rmse = sqrt(mean(evalua$residuo_Dn^2, na.rm = TRUE))
perc_rmse = rmse/mean(evalua$Dn)

ggplot(evalua, aes(x= Dn, y = predicho_Dn))+geom_point()



ggplot(evalua, aes(x= ab_ifn3, y = predicho))+geom_point()
ggplot(evalua, aes(x= Dn_ifn3/dgm_ifn3, y = predicho))+geom_point()
ggplot(evalua, aes(x= Ho_ifn3, y = predicho))+geom_point()
ggplot(evalua, aes(x= Dn_ifn3, y = predicho))+geom_point()

############## inc diam
m.Richards <- nlsLM(
  inc_diam  ~ (a1*Ho_ifn3^a2 + a3*ab_ifn3 + a4*(Dn_ifn3/dgm_ifn3)) * 
    ((1-exp(-b2*Dn_ifn3))^(b3-1))*exp(-b2*Dn_ifn3),
  data = dat_inc %>% filter(inc_an <= quantile(dat_inc$inc_an, 0.90)[[1]] & inc_an >=0.2),
  start = list(a1 = 2.026, a2 = 0.092, a3 = -0.029, a4 = 0.280, b2 = 0.0002835, b3 = 1.4447351),
  control = nls.lm.control(maxiter = 200)
)
summary(m.Richards)
uu <- predict(m.Richards, newdata = dat_inc %>% filter(inc_an <= quantile(dat_inc$inc_an, 0.90)[[1]] & inc_an >=0.2))
evalua <- dat_inc %>%
  filter(inc_an <= quantile(dat_inc$inc_an, 0.90)[[1]] & inc_an >=0.2) %>%
  mutate(predicho = uu) %>%
  mutate(residuos = inc_diam - predicho)

rmse = sqrt(mean(evalua$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(evalua$inc_diam)

ggplot(evalua, aes(x= inc_diam, y = predicho))+geom_point()

