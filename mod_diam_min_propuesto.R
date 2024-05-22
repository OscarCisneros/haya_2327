#este es el modelo descrito en el informe, hay otra aproximación anterior muy similar pero se emplea ésta porque se usan más datos
#modelo de diámetro mínimo a partir del dataframe dat_inc del script "datos_para_mod_inc.R"
# se ha probado a incluir variables de masa en el modelo pero no lo mejora

###################
#modelo de diámetro minimo

#modelo en parcelas regulares
  dat_inc_min_reg <- dat_inc %>%
    #left_join(PCEspParc_Ifn3 %>% select(Estadillo, FPMasa)) %>%
    filter(FPMasa != 4) %>% #sólo parcelas regulares
    group_by(Estadillo) %>%
    summarise(diam_min = min(Dn_ifn3),
              Ho = max(Ho_ifn3),
              Dg = max(dgm_ifn3),
              N = max(n_ifn3),
              G = max(ab_ifn3)) %>%
    ungroup()
  
  #lm.diam_min <- lm(diam_min ~ Ho+Dg+N+G, data = dat_inc_min_reg) #
  lm.diam_min_reg <- lm(diam_min ~ Dg, data = dat_inc_min_reg)
  
  #estadísticos de ajuste
  ajuste_dmin <- data.frame(observado = dat_inc_min_reg$diam_min,
                          predicho = predict(lm.diam_min_reg),
                          residuos = resid(lm.diam_min_reg)) 
  
  #Estadísticos
  estad_ajuste_dmin <- data.frame(
    modelo = "dmin",
    SSE = round(sum((ajuste_dmin$residuos)^2),1),
    SST = round(sum((dat_inc_min_reg$diam_min-mean(dat_inc_min_reg$diam_min))^2),1),
    nparms = 2) %>%
    mutate(
      e = round(mean(ajuste_dmin$residuos),4),
      R2 = round((1-SSE/SST)*100,2),
      RMCE = round(sqrt(SSE/(nrow(dat_inc_min_reg)-nparms)),3)) %>%
    mutate(e_perc = round(e*100/mean(dat_inc_min_reg$diam_min),4),
           RMCE_perc = round(RMCE*100/mean(dat_inc_min_reg$diam_min),3)) %>%
    select(modelo, e, e_perc, RMCE, RMCE_perc, R2)
  
  #summary(lm.diam_min_reg)
  save(lm.diam_min_reg, file = "resultados/mod_diam_min/lm_dim_min_reg")
  
  #modelo en parcelas regulares
  dat_inc_min_irre <- dat_inc %>%
    #left_join(PCEspParc_Ifn3 %>% select(Estadillo, FPMasa)) %>%
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
  
