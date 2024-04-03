#relación altura ~ diámetro para el parámetro balmod_gih del cálculo de
#madera de sierra en transformación a irregular
#no se tiene en cuenta calidad ni parámetros de masa
#puede que no sea correcto este planteamiento para masas irregulares

library(minpack.lm)
library(tidyverse)


Pares_Dn_Ht <- PCMayores_Ifn4 %>%
  filter(Especie == 71 & Provincia == 31 & Forma <= 3) %>%
  mutate(Dn = (Dn1+Dn2)/20) %>%
  select(Dn, Ht, Forma) %>%
  na.omit() 

ggplot(Pares_Dn_Ht, aes(x=Dn, y = Ht, col = as.factor(Forma)))+geom_point()

d_h_nlm <- nlsLM(Ht ~ 1.3 + exp(a+b/Dn),
                        data = Pares_Dn_Ht,
                        start = c (a = 3.42568, b = -14.2348))
summary(d_h_nlm)
predichos <- data.frame(Dn = seq(7.5, 100, by = 5)) %>%
  mutate(Ht_pred = predict(d_h_nlm, newdata = data.frame(Dn = Dn)))

save(d_h_nlm, file = "datos/mod_altura_diametro")
