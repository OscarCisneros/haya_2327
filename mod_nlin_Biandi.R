#modelo de incremento medio anual de área basimétrica individual según un modelo no lineal 
#Según Biachi et al. 2023 (sin efectos aleatorios)

library(minpack.lm)
library(quantreg)
library(tidyverse)

#eliminar los datos con incrementos menores al 0.1 o mayores al 0.9 para cada clase de 2 cm de diámetro
#clases de 2 cm de diámetro
breaks_2cm <- seq(0,max(dat_inc$Dn_ifn3+1), by=2)
corte_diam_2cm <- cut(dat_inc$Dn_ifn3, breaks_2cm)

dat_inc_1 <- dat_inc %>%
  mutate(ab_ind_ifn3 = pi*(Dn_ifn3/2)^2,
         ab_ind = pi*(Dn/2)^2) %>%
  mutate(inc_ab_ind = (ab_ind - ab_ind_ifn3)/anno) %>%
  mutate(clase_2cm = corte_diam_2cm) %>%
  mutate(clase_2cm = ifelse(Dn_ifn3 >= 65, "mayor_65", as.character(clase_2cm))) %>%
  group_by(clase_2cm) %>%
  mutate(dentro = ifelse(inc_ab_ind <= quantile(inc_ab_ind, 0.9)[[1]] & inc_ab_ind >= quantile(inc_ab_ind, 0.1)[[1]],
                         "dentro", "fuera")) %>%
  ungroup() %>%
  filter(dentro == "dentro")

ggplot(dat_inc_1, aes(x= Dn_ifn3, y=inc_ab_ind))+geom_point()
################################################################################  
mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) +
                                            b3*ab_ifn3 + b4*ab_may_ifn3),
                        data = dat_inc_1,
                        start = c (b1 = -0.00550, b2 = 0.87250,
                                   b3 = -0.25327, b4 = -0.20389))
summary(mod.Biandi_nlm)

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)

################################################################################  
mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) +
                                            b3*ab_ifn3 + b4*ab_may_ifn3),
                        data = dat_inc_1,
                        start = c (b1 = -0.00550, b2 = 0.87250,
                                   b3 = -0.25327, b4 = -0.20389))
summary(mod.Biandi_nlm)

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)



################################################################################  
mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) +
                                            b3*log(ab_ifn3 + 1) + b4*ab_may_ifn3/sqrt(Dn_ifn3+1)),
                        data = dat_inc_1,
                        start = c (b1 = -0.00550, b2 = 0.87250,
                                   b3 = -0.25327, b4 = -0.20389))
summary(mod.Biandi_nlm)

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)

################################################################################  
mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) +
                                            b3*log(ab_ifn3 + 1) + b4*ab_may_ifn3/sqrt(Dn_ifn3+1)),
                        data = dat_inc_1,
                        start = c (b1 = -0.00550, b2 = 0.87250,
                                   b3 = -0.25327, b4 = -0.20389),
                        weights = wfct(1/inc_ab_ind))
summary(mod.Biandi_nlm)

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)

################################################################################  
mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) +
                            b3*ab_ifn3 + b4*ab_may_ifn3),
            data = dat_inc_1,
            start = c (b1 = -0.00550, b2 = 0.87250,
                       b3 = -0.25327, b4 = -0.20389),
            weights = wfct(1/inc_ab_ind))
summary(mod.Biandi_nlm)
   
resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)

################################################################################
mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) + b22*Dn_ifn3^2+
                                            b3*ab_ifn3 + b4*ab_may_ifn3),
                        data = dat_inc_1,
                        start = c (b1 = 0.0761091206, b2 = 0.1867479120, b22 = -0.0004551316,
                                   b3 = -0.0085088186, b4 = -0.0077477852))
summary(mod.Biandi_nlm)

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)

#guardar el modelo de incremento de área basimétrica individual
save(mod.Biandi_nlm, file = "datos/mod_inc_area_basim_indiv")
################################################################################
mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) + b22*Dn_ifn3^2+
                                            b3*ab_ifn3 + b4*ab_may_ifn3),
                        data = dat_inc_1,
                        start = c (b1 = -0.00550, b2 = 0.87250, b22 = 0.005,
                                   b3 = -0.25327, b4 = -0.20389),
                        weights = wfct(1/inc_ab_ind))
summary(mod.Biandi_nlm)

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)


resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=predicho, y = inc_ab_ind))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()

ggplot(resultado, aes(x=Dn_ifn3, y = predicho))+
  geom_point()

###############################################################################
#Regresión cuantílica para establecer clases de crecimiento. Se emplean como curvas
#de referencia las relaciones en los cuantiles 10, 30, 50, 70, 90, como representación
#de las bandas de crecimiento 0-20, 20-40, 40-60, 60-80, 80-100.

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

rq_50 = rq(formula = inc_ab_ind ~ predicho, tau = 0.5, data = resultado)
#rqss_50 = rqss(formula = inc_ab_ind ~ predicho, tau = 0.5, data = resultado)
summary(rq_50)
#summary(rqss_50)

funcion_rq <- function(tau_ = 0.5) {
  rq_ = rq(formula = inc_ab_ind ~ predicho, tau = tau_, data = resultado)
  return(data.frame(cuantil = tau_,
                    intercept = coefficients(rq_)[[1]],
                    pendiente = coefficients(rq_)[[2]]))
}
#archivo de corrección del incremento en área basimetrica según la calidad
correc_calidad_irreg <- map_dfr(c(0.1,0.3,0.5,0.7,0.9), ~ funcion_rq(.[1]))
save(correc_calidad_irreg, file = "datos/correc_calidad_irreg")

ggplot(resultado, aes(x=predicho, y = inc_ab_ind))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline(slope = correc_calidad_irreg$pendiente[[1]], intercept = correc_calidad_irreg$intercept[[1]], color = "red", size = 1)+
  geom_abline(slope = correc_calidad_irreg$pendiente[[2]], intercept = correc_calidad_irreg$intercept[[2]], color = "green", size = 1)+
  geom_abline(slope = correc_calidad_irreg$pendiente[[3]], intercept = correc_calidad_irreg$intercept[[3]], color = "orange", size = 1)+
  geom_abline(slope = correc_calidad_irreg$pendiente[[4]], intercept = correc_calidad_irreg$intercept[[4]], color = "blue", size = 1)+
  geom_abline(slope = correc_calidad_irreg$pendiente[[5]], intercept = correc_calidad_irreg$intercept[[5]], color = "darkgreen", size = 1)



################################################################################
#Aproximación al parámetro "q" de Liocourt. Se analiza la relación entre número
#de pies y diámetro, para clases diamétrica de 1 cm. Se analiza sólo en masas
#irregulares (FPMasa == 4)

dat_inc_1
dat_inc
breaks_5cm = seq(2.5,102.5, by = 5)
dat_inc_2 <- dat_inc %>%
  filter(FPMasa == 4) %>%
  #mutate(clase_D = round(Dn_ifn3)) %>%
  mutate(clase_D = cut(Dn_ifn3, breaks = breaks_5cm, labels = seq(5,100, by=5))) %>%
  mutate(clase_D = as.numeric(as.character(clase_D))) %>%
  group_by(Provincia, Estadillo, clase_D) %>%
  summarise(cuenta = sum(factor_exp))
         
ggplot(dat_inc_2, aes(x=clase_D, y = cuenta))+geom_point()

lm_q = lm(log(cuenta) ~ clase_D, data = dat_inc_2)

summary(lm_q)

evol_N <- data.frame(clase_D = seq(10,100, by = 5)) %>%
  mutate(predicho = exp(predict(lm_q, newdata = data.frame(clase_D = seq(10,100, by = 5))))) %>%
  mutate(proporcion = predicho/lead(predicho))
mean(evol_N$proporcion, na.rm = TRUE)
ggplot(evol_N, aes(x=clase_D, y = predicho))+geom_point()

q_90 = rq(formula = log(cuenta) ~ clase_D, tau = 0.999, data = dat_inc_2)
summary(q_90)
ggplot(dat_inc_2, aes(x=clase_D, y = cuenta))+geom_point()+
  geom_line(aes(x=clase_D, y=exp(predict(q_90))))

evol_N <- data.frame(clase_D = seq(10,75, by = 5)) %>%
  mutate(predicho = exp(predict(q_90, newdata = data.frame(clase_D = seq(10,75, by = 5))))) %>%
  mutate(proporcion = predicho/lead(predicho))
a_liocourt = mean(evol_N$proporcion, na.rm = TRUE)  
save(a_liocourt, file = "datos/a_liocourt_irreg")
  
ggplot(evol_N, aes(x=clase_D, y = predicho))+geom_point()

library(frontier)

sfa_q <- sfa( log(cuenta) ~ clase_D,
                    data = dat_inc_2)
summary(sfa_q)
evol_N <- data.frame(clase_D = seq(10,75, by = 5)) %>%
  mutate(predicho = exp(coefficients(sfa_q)[[1]])*exp(coefficients(sfa_q)[[2]]*clase_D)) %>%
  mutate(proporcion = predicho/lead(predicho))
mean(evol_N$proporcion, na.rm = TRUE) 

ggplot(dat_inc_2, aes(x= clase_D, y =log(cuenta) ))+geom_point()+
  geom_line(aes(x=clase_D, y=predict(sfa_q)))



# 
# 
mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3)),
                        data = dat_inc_1,
                        start = c (b1 = -0.0074226, b2 = 2.3531965))
summary(resid(mod.Biandi_nlm))

mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3)),
                        data = dat_inc_1,
                        start = c (b1 = -0.0074226, b2 = 2.3531965),
                        weights = wfct(error^2))
# resultado <- data.frame(fitted_ = fitted(mod.Biandi_nlm),
#                         predicho = predict(mod.Biandi_nlm)) 

resultado <- data.frame(Dn_ifn3 = dat_inc_1$Dn_ifn3,
                        inc_ab_ind = dat_inc_1$inc_ab_ind,
                        predicho = predict(mod.Biandi_nlm, newdata = data.frame(Dn_ifn3 = dat_inc_1$Dn_ifn3))) %>%
  mutate(residuos = inc_ab_ind - predicho )

rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)
summary(mod.Biandi_nlm)
ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)
# 
# 
# resultado <- dat_inc %>%
#   ungroup %>%
#   mutate(predicho = predict(fm1)) %>%
#   mutate(perc95 = ifelse(inc_an <= quantile(dat_inc_1$inc_an, 0.95)[[1]],1,0))
# 
# ggplot(resultado, aes(x=inc_an, y = predicho, col = as.factor(perc95)))+
#   geom_point()+
#   geom_smooth(stat = "smooth")+
#   geom_abline()
# 
# 
