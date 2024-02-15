PCMayores_Ifn4_haya_depura_1 <- PCMayores_Ifn4 %>%
  mutate(Dn = (Dn1+Dn2)/20) %>%
  mutate(factor_exp = case_when(
    Dn < 12.5 ~ 10000/(pi*5^2), 
    Dn >= 12.5 & Dn < 22.5 ~ 10000/(pi*10^2),
    Dn >= 22.5 & Dn < 42.5 ~ 10000/(pi*15^2),
    Dn >= 42.5 ~ 10000/(pi*25^2)
  )) %>%
  group_by(Provincia, Estadillo, Especie) %>%
  summarise(ab = sum(factor_exp*pi*(Dn/200)^2),
            dgm = sqrt(sum(factor_exp*Dn^2)/sum(factor_exp)),
            n = sum(factor_exp),
            n_mort = sum(factor_exp*(OrdenIf4== 444 | OrdenIf4== 888))) %>% # se contabilizan los árboles muertos, proporcionalmente
  ungroup() %>%
  na.omit() %>%
  group_by(Estadillo) %>%
  mutate(ab_perc = ab/sum(ab)) %>%
  mutate(retener = ifelse(71 %in% Especie,1,0)) %>%
  mutate(ab_haya_perc = (Especie ==71)*ab_perc) %>%
  filter(sum(ab_haya_perc)>= ab_limite) %>%
  ungroup() %>%
  filter(ab_haya_perc==1) %>%
  select(-retener, -ab_haya_perc) 


PCMayores_Ifn4_haya_depura_1 <- PCMayores_Ifn4 %>%
  mutate(Dn = (Dn1+Dn2)/20) %>%
  mutate(factor_exp = case_when(
    Dn < 12.5 ~ 10000/(pi*5^2), 
    Dn >= 12.5 & Dn < 22.5 ~ 10000/(pi*10^2),
    Dn >= 22.5 & Dn < 42.5 ~ 10000/(pi*15^2),
    Dn >= 42.5 ~ 10000/(pi*25^2)
  )) %>%
  group_by(Provincia, Estadillo, Especie) %>%
  arrange(desc(Dn)) %>%
  mutate(cum_ = cumsum(factor_exp)) %>%
  mutate(cum_1 = cum_-100) %>%
  mutate(signo_cum_= sign(cum_1)) %>%
  mutate(diff_ = c(0, diff(signo_cum_))) %>%
  mutate(sign_1 = signo_cum_ - diff_ ) %>%
  mutate(mult = (signo_cum_ == -1)*factor_exp + (diff_ == 2)*cum_1) %>%
  summarise(ab = sum(factor_exp*pi*(Dn/200)^2),
          dgm = sqrt(sum(factor_exp*Dn^2)/sum(factor_exp)),
          Ho = sum(mult*Ht)/sum(mult),
          n = sum(factor_exp),
          n_mort = sum(factor_exp*(OrdenIf4== 444 | OrdenIf4== 888))) %>% # se contabilizan los árboles muertos, proporcionalmente
  ungroup()  
