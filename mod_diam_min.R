#modelo de diámetro mínimo


################################################################################
########### Modelo de incremento diametral, árbol individual
################################################################################

#comparación de crecimiento diametral entre IFN3 e IFN4

PCMayores_Ifn4_haya_compara <- PCMayores_Ifn4 %>%
  filter(paste(Provincia, Estadillo, Especie) %in% paste(PCMayores_Ifn4_haya_depura_2$Provincia,
                                                         PCMayores_Ifn4_haya_depura_2$Estadillo,
                                                         PCMayores_Ifn4_haya_depura_2$Especie)) %>% #datos depurados
  filter(!(as.character(OrdenIf4) %in% c("000","888","444"))) %>% #árboles del IFN3 que no se encuentran en el IFN4
  mutate(Dn = (Dn1+Dn2)/20)

PCMayores_Ifn3_haya_compara <- PCMayores_Ifn3 %>%
  filter(paste(Estadillo, Especie) %in% paste(PCMayores_Ifn4_haya_depura_2$Estadillo,
                                              PCMayores_Ifn4_haya_depura_2$Especie)) %>% #filtrar a las parcelas analizadas en IFN4
  filter(!(as.character(OrdenIf3) %in% c("000","888","444"))) %>% #árboles del IFN2 que no se encuentran en el IFN3
  mutate(Dn_ifn3 = (Dn1+Dn2)/20) %>%
  mutate(factor_exp = case_when(
    Dn_ifn3 < 12.5 ~ 10000/(pi*5^2), 
    Dn_ifn3 >= 12.5 & Dn_ifn3 < 22.5 ~ 10000/(pi*10^2),
    Dn_ifn3 >= 22.5 & Dn_ifn3 < 42.5 ~ 10000/(pi*15^2),
    Dn_ifn3 >= 42.5 ~ 10000/(pi*25^2)
  )) %>%
  group_by(Estadillo, Especie) %>%
  arrange(desc(Dn_ifn3)) %>%
  mutate(cum_ = cumsum(factor_exp)) %>%
  mutate(cum_1 = cum_-100) %>%
  mutate(signo_cum_= sign(cum_1)) %>%
  mutate(diff_ = c(0, diff(signo_cum_))) %>%
  mutate(sign_1 = signo_cum_ - diff_ ) %>%
  mutate(mult = (signo_cum_ == -1)*factor_exp + (diff_ == 2)*cum_1) %>%
  mutate(ab_ifn3 = sum(factor_exp*pi*(Dn_ifn3/200)^2),
         dgm_ifn3 = sqrt(sum(factor_exp*Dn_ifn3^2)/sum(factor_exp)),
         Ho_ifn3 = sum(mult*Ht)/sum(mult),
         n_ifn3 = sum(factor_exp)) %>% # se contabilizan los árboles muertos, proporcionalmente
  ungroup() %>%
  select(Estadillo, Especie, OrdenIf3, Dn_ifn3, ab_ifn3, dgm_ifn3, n_ifn3, Ho_ifn3 )

PCMayores_Ifn4_3_haya_compara <- PCMayores_Ifn4_haya_compara %>%
  left_join(PCMayores_Ifn3_haya_compara) %>%
  left_join(PCParcelas_tiempo %>% select(Provincia, Estadillo, anno))

#datos para el modelo de incremento diametral individual
dat_inc <- PCMayores_Ifn4_3_haya_compara %>%
  select(Provincia, Estadillo, OrdenIf3, OrdenIf4, Especie, Ht, Dn,
         Dn_ifn3, ab_ifn3, dgm_ifn3, n_ifn3, Ho_ifn3, anno) %>%
  mutate(inc_diam = Dn - Dn_ifn3) %>%
  mutate(inc_an = inc_diam/anno) %>%
  filter(inc_diam > 0)



###################
#modelo de diámetro minimo

#modelo en parcelas regulares
  dat_inc_min_reg <- dat_inc %>%
    left_join(PCEspParc_Ifn3 %>% select(Estadillo, FPMasa)) %>%
    filter(FPMasa != 4) %>% #sólo parcelas regulares
    group_by(Estadillo) %>%
    summarise(diam_min = min(Dn_ifn3),
              Ho = max(Ho_ifn3),
              Dg = max(dgm_ifn3),
              N = max(n_ifn3),
              G = max(ab_ifn3)) %>%
    ungroup()
  
  #lm.diam_min <- lm(diam_min ~ Ho+Dg+N+G, data = dat_inc_min)
  lm.diam_min_reg <- lm(diam_min ~ Dg, data = dat_inc_min_reg)
  #summary(lm.diam_min_reg)
  save(lm.diam_min_reg, file = "resultados/mod_diam_min/lm_dim_min_reg")
  
  #modelo en parcelas regulares
  dat_inc_min_irre <- dat_inc %>%
    left_join(PCEspParc_Ifn3 %>% select(Estadillo, FPMasa)) %>%
    filter(FPMasa == 4) %>% #sólo parcelas irregulares
    group_by(Estadillo) %>%
    summarise(diam_min = min(Dn_ifn3),
              Ho = max(Ho_ifn3),
              Dg = max(dgm_ifn3),
              N = max(n_ifn3),
              G = max(ab_ifn3)) %>%
    ungroup()
  
  #lm.diam_min <- lm(diam_min ~ Ho+Dg+N+G, data = dat_inc_min)
  lm.diam_min_irreg <- lm(diam_min ~ Dg, data = dat_inc_min_irre)
  #summary(lm.diam_min_irreg)
  save(lm.diam_min_irreg, file = "resultados/mod_diam_min/lm_dim_min_irreg")
  
