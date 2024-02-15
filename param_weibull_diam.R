#Modelizar parámetros de la distribución de Weibull para la distribución diamétrica
#Se depuran los datos según el código "mortalidad_haya_4oIFN.R", con el resultado de "PCMayores_Ifn4_haya_depura_2"


library(univariateML)
library(tidyverse)
library(dlookr)

PCMayores_Ifn4_haya_expand <- PCMayores_Ifn4 %>%
  filter(paste(Provincia, Estadillo, Especie) %in% paste(PCMayores_Ifn4_haya_depura_2$Provincia,
                                                         PCMayores_Ifn4_haya_depura_2$Estadillo,
                                                         PCMayores_Ifn4_haya_depura_2$Especie)) %>% #datos depurados
  filter(!(as.character(OrdenIf4) %in% c("000","888","444"))) %>% #árboles del IFN3 que no se encuentran en el IFN4
  mutate(Dn = (Dn1+Dn2)/20) %>%
  mutate(factor_exp = case_when(
    Dn < 12.5 ~ 10000/(pi*5^2), 
    Dn >= 12.5 & Dn < 22.5 ~ 10000/(pi*10^2),
    Dn >= 22.5 & Dn < 42.5 ~ 10000/(pi*15^2),
    Dn >= 42.5 ~ 10000/(pi*25^2)
  ))

funcion_param_weibul <- function(Provincia_ = 31, Estadillo_ = 9, Especie_ = 71) {
  PCMayores_Ifn4_haya_expand_ <- PCMayores_Ifn4_haya_expand %>%
    filter(paste(Provincia, Estadillo, Especie) == paste(Provincia_, Estadillo_, Especie_)) %>%
    rowwise() %>%
    mutate(lista_ = list(c(Dn, Ht, factor_exp))) %>%
    ungroup() 
  
    #función para expandir la relación de di  
    funcion_expand_diam <- function(D = 98.7, Ht= 14.8, f_exp=5.092958) {
      df.exp <- data.frame(D = rep(D, as.integer(f_exp)),
                           Ht =rep(Ht, as.integer(f_exp)))
      
    }
     
   oo <-  map_dfr(PCMayores_Ifn4_haya_expand_$lista_, ~ funcion_expand_diam(D = .[1], Ht = .[2], f_exp = .[3]))
   
}

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
      
      ggplot(dat_inc, aes(x=log(inc_an)))+geom_density()
      
      lm.inc <- lm(log(inc_diam) ~ I(Dn_ifn3)+ I(Dn_ifn3^2) + log(Dn_ifn3/dgm_ifn3) + Ho_ifn3+ab_ifn3, data = dat_inc)
      summary(lm.inc)   
      
      lm.inc <- lm(inc_diam ~ I(Dn_ifn3)+ I(Dn_ifn3*log(Dn_ifn3)) + I(Dn_ifn3/dgm_ifn3) + Ho_ifn3+ab_ifn3, data = dat_inc)
      lm.inc <- lm(inc_diam ~ I(Dn_ifn3)+ I(Dn_ifn3*log(Dn_ifn3)) + I(Dn_ifn3^2)+ I(Dn_ifn3/dgm_ifn3) + Ho_ifn3+ab_ifn3, data = dat_inc)
      summary(lm.inc)   
      
      pred_ <- predict(lm.inc, newdata = dat_inc)

    evalua <- dat_inc %>%
      mutate(predicho = pred_) %>%
      mutate(residuo = inc_diam-predicho)

    ggplot(evalua, aes(x=inc_diam, y = predicho))+geom_point()
    