#modelo de incremento medio anual de área basimétrica individual según un modelo no lineal 
#Según Biachi et al. 2023 (sin efectos aleatorios)

library(minpack.lm)
library(quantreg)
library(ggpubr)
library(tidyverse)

#eliminar los datos con incrementos menores al 0.2 o mayores al 0.9 para cada clase de 2 cm de diámetro
#clases de 2 cm de diámetro
#se consigue un incremento medio de 0.2 cm/año en diámetro, lo que se recoge en gofagus (pp 73) y de media en H5 (más bien por debajo de H5)
breaks_2cm <- seq(0,max(dat_inc$Dn_ifn3+1), by=2)
corte_diam_2cm <- cut(dat_inc$Dn_ifn3, breaks_2cm)

dat_inc_1 <- dat_inc %>%
  mutate(ab_ind_ifn3 = pi*(Dn_ifn3/2)^2,
         ab_ind = pi*(Dn/2)^2) %>%
  mutate(inc_ab_ind = (ab_ind - ab_ind_ifn3)/anno) %>%
  mutate(clase_2cm = corte_diam_2cm) %>%
  mutate(clase_2cm = ifelse(Dn_ifn3 >= 65, "mayor_65", as.character(clase_2cm))) %>%
  group_by(clase_2cm) %>%
  mutate(dentro = ifelse(inc_ab_ind <= quantile(inc_ab_ind, 0.9)[[1]] & inc_ab_ind >= quantile(inc_ab_ind, 0.2)[[1]],
                         "dentro", "fuera")) %>%
  ungroup() %>%
  filter(dentro == "dentro")

ggplot(dat_inc_1, aes(x= Dn_ifn3, y=inc_ab_ind))+geom_point()
summary(dat_inc_1$inc_ab_ind)
sd(dat_inc_1$inc_ab_ind)

library(ggExtra)

p = ggplot(dat_inc_1 , aes(x= Dn_ifn3, y = inc_ab_ind ))+
  geom_point(color = "blue")+
  #geom_vline(xintercept = mort_5_*100, linetype="dashed",  color = "red", linewidth=1.5)+
  #annotate("label", size = 10, x = mort_5_*100+5,y = 25, label = paste0("mortalidad media en 5 años: ", round(mort_5_*100, 3)," (%)"))+
  ggtitle("Relación entre diámetro e incremento anual en área basimétrica", subtitle = "Incremento entre IFN3 e IFN4")+
  #scale_x_continuous(breaks = seq(0, 120, by = 5))+
  theme_light()+
  labs(x = "Diámetro normal (cm)", y = "Incremento anual en área basimétrica (cm2)")+
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme(plot.title = element_text(family = "sans", margin=margin(0,0,30,0)))+
  theme(text = element_text(size = 40)) +
  theme(plot.subtitle=element_text(size=30, face="italic", margin=margin(0,0,20,0)))

ggMarginal(p, type = "boxplot", color = "slateblue1", fill = "slateblue4") 


ggsave("informe/graf/inc_ab.png", width = 677.4 , height = 364.416, units = "mm")

################################################################################  

mod.Biandi_nlm <- nlsLM(inc_ab_ind ~ exp( b1*Dn_ifn3+ b2*log(Dn_ifn3) + b22*Dn_ifn3^2+
                                            b4*ab_may_ifn3),
                        data = dat_inc_1,
                        start = c (b1 = 0.0761091206, b2 = 0.1867479120, b22 = -0.0004551316,
                                   b4 = -0.0077477852))
summary(mod.Biandi_nlm)

resultado <- dat_inc_1 %>%
  ungroup %>%
  mutate(predicho = predict(mod.Biandi_nlm)) %>%
  mutate(residuos = inc_ab_ind - predicho)

#comparar con los crecimientos en diámetros
res_comprobar <- resultado %>% 
  mutate( Dn_ifn3_recal = sqrt((inc_ab_ind+pi*(Dn_ifn3/2)^2)*4/pi))
summary(res_comprobar$Dn_ifn3_recal-res_comprobar$Dn_ifn3)

#crecimiento en la bbdd (filtrada por cuantiles)
summary(dat_inc_1$inc_an)


rmse = sqrt(mean(resultado$residuos^2, na.rm = TRUE))
perc_rmse = rmse/mean(resultado$inc_ab_ind)

ggplot(resultado, aes(x=inc_ab_ind, y = predicho))+
  geom_point()+
  geom_smooth(stat = "smooth")+
  geom_abline()
plot(mod.Biandi_nlm)

#estadísticos de ajuste
ajuste_ab <- data.frame(observado = dat_inc_1$inc_ab_ind,
                        predicho = predict(mod.Biandi_nlm),
                        residuos = resid(mod.Biandi_nlm)) 

#Estadísticos
estad_ajuste_ab <- data.frame(
  modelo = "Inc_ind_ab_an",
  SSE = round(sum((ajuste_ab$residuos)^2),1),
  SST = round(sum((dat_inc_1$inc_ab_ind-mean(dat_inc_1$inc_ab_ind))^2),1),
  nparms = 5) %>%
  mutate(
    e = round(mean(ajuste_ab$residuos),4),
    R2 = round((1-SSE/SST)*100,2),
    RMCE = round(sqrt(SSE/(nrow(dat_inc_1)-nparms)),3)) %>%
  mutate(e_perc = round(e*100/mean(dat_inc_1$inc_ab_ind),4),
         RMCE_perc = round(RMCE*100/mean(dat_inc_1$inc_ab_ind),3)) %>%
  select(modelo, e, e_perc, RMCE, RMCE_perc, R2)


#Gráficos de ajuste
graf_ajuste <- data.frame(observado = dat_inc_1$inc_ab_ind,
                          predicho = predict(mod.Biandi_nlm),
                          residuo = resid(mod.Biandi_nlm)) 
res_pred <- ggplot(graf_ajuste, aes(x=predicho, y=residuo))+
  geom_point(shape=4, size = 4, color= "darkblue", stroke=1)+
  stat_ellipse(level = 0.95, color = "orangered", linewidth = 4)+
  ggtitle("Residuos vs Predichos")+
  geom_hline(yintercept=0,  linewidth = 0.5)+
  xlim(min(graf_ajuste$predicho),max(graf_ajuste$predicho))+
  theme_light()+
  theme(text = element_text(size = 40))

ggsave("informe/graf/res_pred_ajuste_inc_ab.png", width = 677.4 , height = 364.416, units = "mm")

obs_pred <- ggplot(graf_ajuste, aes(x=observado, y=predicho))+
  geom_point(shape=4, size = 4, color= "darkblue", stroke=1)+
  stat_ellipse(level = 0.95, color = "orangered", linewidth = 4)+
  ggtitle("Observados vs Predichos")+
  geom_abline(intercept = 0, slope = 1, linewidth = 0.5)+
  xlim(min(graf_ajuste$observado),max(graf_ajuste$observado))+
  theme(text = element_text(size = 40))+
  theme_light()+
  theme(text = element_text(size = 40))

ggsave("informe/graf/obs_pred_ajuste_inc_ab.png", width = 677.4 , height = 364.416, units = "mm")

figure <- ggarrange(res_pred, obs_pred,
                    #labels = c("A", "B"),
                    ncol = 2, nrow = 1)
figure
ggsave("informe/graf/graf_ajuste_inc_ab.png", width = 677.4 , height = 364.416, units = "mm")

#guardar el modelo de incremento de área basimétrica individual
save(mod.Biandi_nlm, file = "datos/mod_inc_area_basim_indiv")

################################################################################

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
correc_calidad_irreg <- map_dfr(c(0.40,0.45,0.50,0.55,0.60), ~ funcion_rq(.[1]))
save(correc_calidad_irreg, file = "datos/correc_calidad_irreg")

res_comprobar_correc <- resultado %>% 
  mutate(correc_1 = correc_calidad_irreg[1,2]+correc_calidad_irreg[1,3]*inc_ab_ind,
         correc_2 = correc_calidad_irreg[2,2]+correc_calidad_irreg[2,3]*inc_ab_ind,
         correc_3 = correc_calidad_irreg[3,2]+correc_calidad_irreg[3,3]*inc_ab_ind,
         correc_4 = correc_calidad_irreg[4,2]+correc_calidad_irreg[4,3]*inc_ab_ind,
         correc_5 = correc_calidad_irreg[5,2]+correc_calidad_irreg[5,3]*inc_ab_ind) %>%
  mutate( Dn_ifn3_recal = sqrt((inc_ab_ind+pi*(Dn_ifn3/2)^2)*4/pi),
          Dn_correc_1 = sqrt((correc_1+pi*(Dn_ifn3/2)^2)*4/pi),
          Dn_correc_2 = sqrt((correc_2+pi*(Dn_ifn3/2)^2)*4/pi),
          Dn_correc_3 = sqrt((correc_3+pi*(Dn_ifn3/2)^2)*4/pi),
          Dn_correc_4 = sqrt((correc_4+pi*(Dn_ifn3/2)^2)*4/pi),
          Dn_correc_5 = sqrt((correc_5+pi*(Dn_ifn3/2)^2)*4/pi))
summary(res_comprobar_correc$Dn_ifn3_recal - res_comprobar_correc$Dn_correc_1)
summary(res_comprobar_correc$Dn_ifn3_recal - res_comprobar_correc$Dn_correc_2)
summary(res_comprobar_correc$Dn_ifn3_recal - res_comprobar_correc$Dn_correc_3)
summary(res_comprobar_correc$Dn_ifn3_recal - res_comprobar_correc$Dn_correc_4)
summary(res_comprobar_correc$Dn_ifn3_recal - res_comprobar_correc$Dn_correc_5)

res_comprobar_correc <- resultado %>% 
  mutate(correc_1 = correc_calidad_irreg[1,2]+correc_calidad_irreg[1,3]*inc_ab_ind,
         correc_2 = correc_calidad_irreg[2,2]+correc_calidad_irreg[2,3]*inc_ab_ind,
         correc_3 = correc_calidad_irreg[3,2]+correc_calidad_irreg[3,3]*inc_ab_ind,
         correc_4 = correc_calidad_irreg[4,2]+correc_calidad_irreg[4,3]*inc_ab_ind,
         correc_5 = correc_calidad_irreg[5,2]+correc_calidad_irreg[5,3]*inc_ab_ind) %>%
  mutate(perc_correc_1 = (correc_1-inc_ab_ind)/inc_ab_ind,
         perc_correc_2 = (correc_2-inc_ab_ind)/inc_ab_ind,
         perc_correc_3 = (correc_3-inc_ab_ind)/inc_ab_ind,
         perc_correc_4 = (correc_4-inc_ab_ind)/inc_ab_ind,
         perc_correc_5 = (correc_5-inc_ab_ind)/inc_ab_ind)

ggplot(res_comprobar_correc, aes(x=inc_ab_ind, perc_correc_1))+ geom_point()+
  geom_point(aes(x=inc_ab_ind, perc_correc_2))+
  geom_point(aes(x=inc_ab_ind, perc_correc_3))+
  geom_point(aes(x=inc_ab_ind, perc_correc_4))+
  geom_point(aes(x=inc_ab_ind, perc_correc_5))+
  xlim(quantile(res_comprobar_correc$inc_ab_ind, 0.25)[[1]], quantile(res_comprobar_correc$inc_ab_ind, 0.75)[[1]])

res_comprobar_correc_interp <- resultado %>% 
  mutate(correc_1 = correc_calidad_irreg[1,2]+inc_ab_ind,
         correc_2 = correc_calidad_irreg[2,2]+inc_ab_ind,
         correc_3 = correc_calidad_irreg[3,2]+inc_ab_ind,
         correc_4 = correc_calidad_irreg[4,2]+inc_ab_ind,
         correc_5 = correc_calidad_irreg[5,2]+inc_ab_ind) %>%
  mutate(perc_correc_1 = (correc_1-inc_ab_ind)/inc_ab_ind,
         perc_correc_2 = (correc_2-inc_ab_ind)/inc_ab_ind,
         perc_correc_3 = (correc_3-inc_ab_ind)/inc_ab_ind,
         perc_correc_4 = (correc_4-inc_ab_ind)/inc_ab_ind,
         perc_correc_5 = (correc_5-inc_ab_ind)/inc_ab_ind)

ggplot(res_comprobar_correc_interp, aes(x=inc_ab_ind, perc_correc_1))+ geom_point()+
  geom_point(aes(x=inc_ab_ind, perc_correc_2))+
  geom_point(aes(x=inc_ab_ind, perc_correc_3))+
  geom_point(aes(x=inc_ab_ind, perc_correc_4))+
  geom_point(aes(x=inc_ab_ind, perc_correc_5))+
  xlim(quantile(res_comprobar_correc$inc_ab_ind, 0.25)[[1]], quantile(res_comprobar_correc$inc_ab_ind, 0.75)[[1]])
ggplot(resultado, aes(x=predicho, y = inc_ab_ind))+
  geom_point(size = 3)+
  #geom_smooth(stat = "smooth")+
  geom_abline(slope = correc_calidad_irreg$pendiente[[1]], intercept = correc_calidad_irreg$intercept[[1]], color = "red", size = 8)+
  geom_abline(slope = correc_calidad_irreg$pendiente[[2]], intercept = correc_calidad_irreg$intercept[[2]], color = "green", size = 8)+
  geom_abline(slope = correc_calidad_irreg$pendiente[[3]], intercept = correc_calidad_irreg$intercept[[3]], color = "orange", size = 8)+
  geom_abline(slope = correc_calidad_irreg$pendiente[[4]], intercept = correc_calidad_irreg$intercept[[4]], color = "blue", size = 8)+
  geom_abline(slope = correc_calidad_irreg$pendiente[[5]], intercept = correc_calidad_irreg$intercept[[5]], color = "darkgreen", size = 8)+
  ggtitle("Rectas de regresión cuantílica. Predicción del incremento en área basimétrica", subtitle = "Cuantiles: 10,30,50,70,90")+
  theme_light()+
  labs(x = "Predicho", y = "Observado")+
  theme(text = element_text(size = 60)) +
  theme(axis.title.y = element_text(size=40, margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  theme(axis.title.x = element_text(size=40, margin = margin(t = 20, r = 0, b = 0, l = 0))) +
  theme(plot.title = element_text(size=50, family = "sans", margin=margin(0,0,30,0)))+
  theme(plot.subtitle=element_text(size=30, face="italic", margin=margin(0,0,20,0)))

ggsave("informe/graf/correc_calidad_irreg_.png", width = 677.4 , height = 364.416, units = "mm")

#############################################################################
