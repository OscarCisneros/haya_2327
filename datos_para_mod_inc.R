#Generar datos para el modelo de incremento diametral

source("scripts/mortalidad_haya_4oIFN_varias_bbdd.R")

################################################################################
########### Modelo de incremento diametral, árbol individual
################################################################################

#comparación de crecimiento diametral entre IFN3 e IFN4

PCMayores_Ifn4_haya_compara <- PCMayores_Ifn4 %>%
  filter(Cla == "A" & Subclase == 1) %>% #parcelas medidas también en el Ifn3
  filter(paste(Provincia, Estadillo, Especie) %in% paste(PCMayores_Ifn4_haya_depura_2$Provincia,
                                                         PCMayores_Ifn4_haya_depura_2$Estadillo,
                                                         PCMayores_Ifn4_haya_depura_2$Especie)) %>% #datos depurados
  filter(!(as.character(OrdenIf4) %in% c("000","888","444"))) %>% #árboles del IFN3 que no se encuentran en el IFN4
  mutate(Dn = (Dn1+Dn2)/20)

#incluir la forma principal de masa
PCEspParc_Ifn3_compara <- PCEspParc_Ifn3 %>%
  filter(Cla == "A" & Subclase == 1) %>% #parcelas medidas también en el Ifn3
  filter(paste(Provincia, Estadillo, Especie) %in% paste(PCMayores_Ifn4_haya_depura_2$Provincia,
                                                         PCMayores_Ifn4_haya_depura_2$Estadillo,
                                                         PCMayores_Ifn4_haya_depura_2$Especie)) %>%
  distinct(Provincia, Estadillo,FPMasa) %>%
  group_by(Provincia, Estadillo) %>%
  summarise(FPMasa = max(FPMasa, na.rm = TRUE)) %>% #algunas parcelas tienen varios tipos de masa, se conserva el mayor (más irregularidad, revisar)
  ungroup()


PCMayores_Ifn3_haya_compara <- PCMayores_Ifn3 %>%
  filter(Cla == "A" & Subclase == 1) %>% #parcelas medidas también en el Ifn3
  filter(paste(Provincia, Estadillo, Especie) %in% paste(PCMayores_Ifn4_haya_depura_2$Provincia,
                                                         PCMayores_Ifn4_haya_depura_2$Estadillo,
                                                         PCMayores_Ifn4_haya_depura_2$Especie)) %>% #filtrar a las parcelas analizadas en IFN4
  filter(!(as.character(OrdenIf3) %in% c("000","888","444"))) %>% #árboles del IFN2 que no se encuentran en el IFN3
  mutate(Dn_ifn3 = (Dn1+Dn2)/20) %>%
  mutate(factor_exp = case_when(
    Dn_ifn3 < 12.5 ~ 10000/(pi*5^2), 
    Dn_ifn3 >= 12.5 & Dn_ifn3 < 22.5 ~ 10000/(pi*10^2),
    Dn_ifn3 >= 22.5 & Dn_ifn3 < 42.5 ~ 10000/(pi*15^2),
    Dn_ifn3 >= 42.5 ~ 10000/(pi*25^2)
  )) %>%
  group_by(Provincia,Estadillo, Especie) %>%
  arrange(desc(Dn_ifn3)) %>%
  mutate(cum_ = cumsum(factor_exp)) %>%
  mutate(cum_1 = cum_-100) %>%
  mutate(signo_cum_= sign(cum_1)) %>%
  mutate(diff_ = c(0, diff(signo_cum_))) %>%
  mutate(sign_1 = signo_cum_ - diff_ ) %>%
  mutate(mult = (signo_cum_ == -1)*factor_exp + (diff_ == 2)*cum_1) %>%
  mutate(vol = exp(-10.3311+1.99827*log(Dn_ifn3)+0.996055*log(Ht))) %>% #ecuación GOFagus, pag 50
  mutate(ab_ = factor_exp*pi*(Dn_ifn3/200)^2) %>% #cálculo de ab en árboles de más diámetro (ab_may_ifn3)
  mutate(ab_cumsum = cumsum(ab_)) %>% 
  mutate(ab_may_ifn3 = ab_cumsum - factor_exp*pi*(Dn_ifn3/200)^2) %>%
  mutate(ab_ifn3 = sum(factor_exp*pi*(Dn_ifn3/200)^2),
         dgm_ifn3 = sqrt(sum(factor_exp*Dn_ifn3^2)/sum(factor_exp)),
         Ho_ifn3 = sum(mult*Ht)/sum(mult),
         H_Lorey_ifn3 = sum(ab_*Ht)/sum(factor_exp*pi*(Dn_ifn3/200)^2),
         V_ifn3 = sum(factor_exp*vol),
         n_ifn3 = sum(factor_exp)) %>% # se contabilizan los árboles muertos, proporcionalmente
  ungroup() %>%
  select(Provincia,Estadillo, Especie, OrdenIf3, Dn_ifn3, ab_may_ifn3, ab_ifn3, dgm_ifn3, n_ifn3, V_ifn3, Ho_ifn3, H_Lorey_ifn3, factor_exp )

PCMayores_Ifn4_3_haya_compara <- PCMayores_Ifn4_haya_compara %>%
  left_join(PCMayores_Ifn3_haya_compara) %>%
  left_join(PCEspParc_Ifn3_compara) %>%
  left_join(PCParcelas_tiempo %>% select(Provincia, Estadillo, anno))
  

#datos para el modelo de incremento diametral individual
dat_inc <- PCMayores_Ifn4_3_haya_compara %>%
  select(Provincia, Estadillo, OrdenIf3, OrdenIf4, Especie, Ht, Dn,
         Dn_ifn3, ab_may_ifn3, ab_ifn3, dgm_ifn3, n_ifn3, Ho_ifn3, H_Lorey_ifn3, anno, FPMasa, factor_exp) %>%
  mutate(inc_diam = Dn - Dn_ifn3) %>%
  mutate(inc_an = inc_diam/anno) %>%
  filter(inc_diam > 0)



#prueba inc anual
library(tidyverse)
library(minpack.lm)
library(nlraa)

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

################################################################################
################################################################################
#prueba relación Dg ~f(N;Ho)
library(tidyverse)

func_outl <- function(x) {
  lim_sup = quantile(x, 0.75)[[1]]+1.5*IQR(x)
  lim_inf = quantile(x, 0.25)[[1]]-1.5*IQR(x)
  outl = x < lim_inf | x > lim_sup
}

IFN_3_sin_outlier <- PCMayores_Ifn3_haya_compara %>%
  distinct(Provincia,Estadillo, Especie, ab_ifn3, dgm_ifn3, n_ifn3, Ho_ifn3 ) %>%
  mutate(outl_inc = func_outl(dgm_ifn3))
#diagnose_numeric(bb)

############## DG_ sin ouliers
m.DG_m_IFN3 <- lm(log(dgm_ifn3)~ log(n_ifn3)+log(Ho_ifn3),
  data = IFN_3_sin_outlier %>% filter(!outl_inc))
summary(m.DG_m_IFN3)

################################################################################
################################################################################
#prueba relación V ~f(Dg;Ho,N)
library(tidyverse)

func_outl <- function(x) {
  lim_sup = quantile(x, 0.75)[[1]]+1.5*IQR(x)
  lim_inf = quantile(x, 0.25)[[1]]-1.5*IQR(x)
  outl = x < lim_inf | x > lim_sup
}

IFN_3_sin_outlier <- PCMayores_Ifn3_haya_compara %>%
  distinct(Provincia,Estadillo, Especie, ab_ifn3, dgm_ifn3, n_ifn3, V_ifn3, Ho_ifn3 ) %>%
  mutate(outl_inc = func_outl(V_ifn3))
#diagnose_numeric(bb)

############## DG_ sin ouliers
m.V_IFN3 <- lm(log(V_ifn3) ~ log(dgm_ifn3)+log(Ho_ifn3)+log(n_ifn3),
                  data = IFN_3_sin_outlier %>% filter(!outl_inc))
summary(m.V_IFN3)
