#modelo de incremento medio anual del diámetro según un modelo nolineal mixto
#Según Biachi et al. 2023


library(nlme)
library(tidyverse)

dat_inc_1 <- dat_inc %>%
  mutate(id_parc = paste0(Provincia, "_", Estadillo)) %>%
  group_by(id_parc)

Δba = exp (bpi + b1*V1 + ... + bn*Vn ) + εpi.ml

inc_an ~ exp(bpi + b1*Dn_ifn3+ b2*dgm_ifn3 + b3*ab_ifn3 + b4*ab_may_ifn3 + b5*Ho_ifn3+ b6*H_Lorey_ifn3)
fixed = b1 + b2 + b3 + b4 + b5 + b6 ~ 1
random = bpi ~ 1

fm1 <- nlme(inc_an ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) +
                           b3*log(ab_ifn3 + 1) + b4*ab_may_ifn3/sqrt(Dn_ifn3+1)),
            data = dat_inc_1,
            fixed = list(b1 ~ 1, b2 ~ 1, b3 ~ 1, b4 ~ 1),
            groups = ~ id_parc,
            start = c (b1 = -0.00550, b2 = 0.87250,
                      b3 = -0.25327, b4 = -0.20389),
            control = nlmeControl(maxIter  = 200))

summary(fm1)

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(fm1)) %>%
  mutate(perc95 = ifelse(inc_an <= quantile(dat_inc_1$inc_an, 0.95)[[1]],1,0))

ggplot(resultado, aes(x=inc_an, y = predicho, col = as.factor(perc95)))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
# fm1 <- nlme(inc_an ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) +
#                             b3*log(ab_ifn3 + 1)),
#             data = dat_inc_1,
#             fixed = b1 + b2 + b3 ~ 1,
#             groups = ~ id_parc,
#             #random = bpi ~ 1,
#             start = c (b1 = -0.00550, b2 = 0.87250,
#                        b3 = -0.25327),
#             control = nlmeControl(msMaxIter = 200))

