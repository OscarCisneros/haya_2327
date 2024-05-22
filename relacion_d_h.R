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

#estadísticos de ajuste
ajuste_h_d <- data.frame(observado = Pares_Dn_Ht$Ht,
                          predicho = predict(d_h_nlm),
                          residuos = resid(d_h_nlm)) 

ggplot(ajuste_h_d, aes(x= predicho, y= observado))+
  geom_point()+
  stat_ellipse(level = 0.95, color = "orangered", linewidth = 4)


ggplot(Pares_Dn_Ht, aes(x=Dn, y=Ht))+
  geom_point(color = "blue", alpha = 0.5)+
  geom_line(aes(x= Dn, y = predict(d_h_nlm)), color = "darkorange", size=4)


#Estadísticos
estad_ajuste_h_d <- data.frame(
  modelo = "dmin",
  SSE = round(sum((ajuste_h_d$residuos)^2),1),
  SST = round(sum((Pares_Dn_Ht$Ht-mean(Pares_Dn_Ht$Ht))^2),1),
  nparms = 2) %>%
  mutate(
    e = round(mean(ajuste_dmin$residuos),4),
    R2 = round((1-SSE/SST)*100,2),
    RMCE = round(sqrt(SSE/(nrow(Pares_Dn_Ht)-nparms)),3)) %>%
  mutate(e_perc = round(e*100/mean(Pares_Dn_Ht$Ht),4),
         RMCE_perc = round(RMCE*100/mean(Pares_Dn_Ht$Ht),3)) %>%
  select(modelo, e, e_perc, RMCE, RMCE_perc, R2)
