#código para calcular simular la transformación a monte alto irregular
#a partir de datos iniciales en una edad concreta

library(openxlsx)
library(zoo)
library(ggnewscale)
library(tidyverse)

#Datos de partida

  #Duración de la simulación
  t_fin = 200

  #Índice de sitio, se asimilan las calidades mala, media, buena... a las de montes regulares
  IS = 25

  #Datos del monte regular
  N_reg <- 700
  Edad_reg <- 50
  G_reg <- 26
  
  #área basimétrica objetivo
  ab_objetivo = 20
  precis_ab = 0.05 #margen para el ab_objetivo
  #rango de diámetros
  diam_max = 65.5
  diam_min = 7.5
  
  #peso de la intervención, tanto por uno en área basimétrica
  peso_G = 0.25
  #años entre intervenciones, rotación
  rota = 10

  #parámetro q, curva de Liocourt
  load("datos/a_liocourt_irreg")
  
  #modelo de diámetro mínimo
  load("resultados/mod_diam_min/lm_dim_min_reg")
  
  #tarifa de volumen
  load("datos/lm.vcc")
  
  #modelo de incremento en área basimétrica
  load("datos/mod_inc_area_basim_indiv")
  
  #corrección del crecimiento diametral
  load("datos/correc_calidad_irreg")
  correc_calidad_irreg$calidad <- c(13,16,19,22,25)
  int_corr_ab <- correc_calidad_irreg$intercept[which(correc_calidad_irreg$calidad == IS)]
  slo_corr_ab <- correc_calidad_irreg$pendiente[which(correc_calidad_irreg$calidad == IS)]
  
  
  # parámetros relación Dm ~ f(Dg, N, G)
  # se emplea la relación de masas regulares
  a7 = 0.191406636051764
  a8 = -0.000620621044818245
  a10 = 0.0166602839868406
  
  #secuencia de la simulación
  tiempo = seq(1,t_fin)
  
  # Variables "Antes de la clara"
  N_a <- rep(0, length(tiempo))
  Dg_a <- rep(0, length(tiempo))
  dist_D_a <-  vector(mode='list', length=length(tiempo))
  dist_d_cm_a <-  vector(mode='list', length=length(tiempo))
  G_a <- rep(0, length(tiempo))
  V_a <- rep(0, length(tiempo))
  vi_a <- rep(0, length(tiempo))
  H_D_a <- rep(0, length(tiempo))
  IH_a <- rep(0, length(tiempo))
  
  # Variables "Después de la clara"
  N_d <- rep(0, length(tiempo))
  Dg_d <- rep(0, length(tiempo))
  dist_D_d <- vector(mode='list', length=length(tiempo))
  dist_d_cm_d <-  vector(mode='list', length=length(tiempo))
  G_d <- rep(0, length(tiempo))
  V_d <- rep(0, length(tiempo))
  vi_d <- rep(0, length(tiempo))
  H_D_d <- rep(0, length(tiempo))
  IH_d <- rep(0, length(tiempo))
  
  
  # Extraido en la clara
  N_e <- rep(0, length(tiempo))
  Dg_e <- rep(0, length(tiempo))
  G_e <- rep(0, length(tiempo))
  V_e <- rep(0, length(tiempo))
  IH_e <- rep(0, length(tiempo))
  

  
#Determinar la curva objetivo----
  q = log(a_liocourt)/5
  K = (-40000/pi)*ab_objetivo/(exp(-q*diam_max)*(diam_max^2/q+2*diam_max/q^2+2/q^3)-
                                 exp(-q*diam_min)*(diam_min^2/q+2*diam_min/q^2+2/q^3))
  N_x5 <- data.frame(diam_min_ = seq(diam_min, diam_max, by=5)) %>%
    mutate(diam_max_ = diam_min_+5) %>%
    mutate(N_x_y = -K/q*(exp(-q*diam_max_)- exp(-q*diam_min_))) %>%
    mutate(ab_ = pi*((diam_min_+2.5)/200)^2*N_x_y) %>%
    mutate(clase_D = diam_min_+2.5) 
  
  sum(N_x5$ab_)
  sum(N_x5$N_x_y)
  
  #Determinar la curva de inicio----
  
    #función de densidad de Weibull con 3 parámetros
    func_dens_weibull <- function(x,a,b,c) {
      (c/b)*((x-a)/b)^(c-1)*exp(-((x-a)/b)^c)
    }
    # método de Bisección para calcular el parámetro "c" de la ecuación de
    # weibull
    Bisection <- function(f, a, b, n = 1000, tol = 1e-7) {
      if (!(f(a) < 0) && (f(b) > 0)) {
        stop('The root does not exist within this interval')
      } else if (!(f(a) > 0) && (f(b) < 0)) {
        stop('The root does not exist within this interval')
      }
      for (i in 1:n) {
        c <- (a + b) / 2
        if ((f(c) == 0) || ((b - a) / 2) < tol) {
          return(c)
        }
        ifelse(sign(f(c)) == sign(f(a)), 
               a <- c,
               b <- c)
      }
      print('Too many iterations')
    }
    # función de expansión de parámetros, ecuación de Weibull
  
    func_expande_Weibull_irreg <- function ( N_a_, G_a_) {
      N_ = N_a_
      G_=  G_a_
      Dg_ = sqrt(G_*40000/(pi*N_))
      Dm_ = Dg_ - exp(a7+a8*N_+a10*G_)
      var_ = Dg_^2 - Dm_^2
      
      # parámetro a
        newdata_ = data.frame(Dg = Dg_)
        a = as.numeric(predict(lm.diam_min_reg, newdata = newdata_))
        a = round(a/0.5)*0.5 #para empezar en el rango inferior de una clase de 1 cm
      
      # parámetro c
      func = function(x){
        var_ -  ((Dm_ - a)^2 / (gamma(1+1/x))^2) *(gamma(1+2/x) - (gamma(1+1/x))^2)
      }
      c = Bisection(f = func, 0.1, 10) #Bisection(f = func,1,4)
      # parámetro b
      b = (Dm_ - a)/gamma(1+1/c)
      return(c(a,b,c))
    }
    
    breaks_5cm = seq(2.5,152.5, by = 5) #divisiones teóricas de la distribución inicial
    param_weibull_irreg <- func_expande_Weibull_irreg(N_a_ = N_reg, G_a_ = G_reg)
    #param_= func_expande_Weibull(i_, Dg_a_, N_a_) #tiene en cuanta la variación de "a", diámetro mínimo
    
    df_ <- data.frame(d_ = seq(round(param_weibull_irreg[1]/0.5)*0.5,150)) %>% #150 como diámetro superior, pero deberíamos parametrizarlo
      mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_weibull_irreg[1],param_weibull_irreg[2],param_weibull_irreg[3]))* N_a_) %>%
      filter(!is.na(n_d)) %>%
      mutate(n_d = n_d/sum(n_d)*N_a_) %>% #para eliminar errores al aproximar la curva de densidad de Weibull
      mutate(clase_D = cut(d_, breaks = breaks_5cm, labels = seq(5,150,by=5))) %>% #labels identifica el diámetro medio de la clase
      mutate(clase_D = as.numeric(as.character(clase_D))) %>%
      mutate(ab = pi*(d_/200)^2*n_d) %>%
      mutate(vol = predict(lm.vcc, newdata = data.frame(Dn_mm = d_*10))/1000*n_d)
    
    df_clase <- df_ %>%
      group_by(clase_D) %>%
      summarise(N_antes = sum(n_d),
                G_antes = sum(ab),
                V_antes = sum(vol)) %>%
      ungroup() %>%
      mutate(extra_ = (clase_D >= max(N_x5$clase_D))) %>%
      mutate(suma_extra_N = sum(N_antes*extra_),
             suma_extra_G = sum(G_antes*extra_),
             suma_extra_V = sum(V_antes*extra_)) %>%
      mutate(N_antes = ifelse(clase_D == max(N_x5$clase_D), suma_extra_N, N_antes),
             G_antes = ifelse(clase_D == max(N_x5$clase_D), suma_extra_G, G_antes),
             V_antes = ifelse(clase_D == max(N_x5$clase_D), suma_extra_V, V_antes)) %>%
      filter(clase_D %in% N_x5$clase_D) %>%
      select(-extra_, -suma_extra_N,-suma_extra_G,-suma_extra_V) 
  
  #variables dasocráticas iniciales
    N_a[1] = sum(df_clase$N_antes)
    G_a[1] = sum(df_clase$G_antes)
    Dg_a[1] = sqrt(G_a[1]*(40000/(pi*N_a[1])))
    dist_D_a[[1]] = df_clase
    dist_d_cm_a[[1]] = df_
    V_a[1] = sum(df_clase$V_antes)
  # N_a[1] = sum(df_$n_d)
  # Dg_a[1] = sqrt(sum(df_$n_d*(df_$d_)^2)/sum(df_$n_d))
  # dist_D_a[1] = df_
  # G_a[1] = sum(df_$ab)
  # V_a[1] = sum(df_$vol)
    
  funcion_actualizar_despues <- function(i)  {
    #unir la distribución objetivo y la actual
    distribucion_act <- N_x5 %>%
      left_join(dist_D_a[[i]]) 
    
    #determinar si se interviene y en su caso calcular
    if (G_a[i] <= ab_objetivo*(1+precis_ab)) { #no se interviene
      print("primera_condicion")
      N_d[i] <<- N_a[i]
      Dg_d[i] <<- Dg_a[i]
      dist_D_d[[i]] <<- dist_D_a[[i]]
      dist_d_cm_d[[i]] <<- dist_d_cm_a[[i]]
      G_d[i] <<- G_a[i]
      V_d[i] <<- V_a[i]
    } else if (tiempo[i] %% rota !=0  & tiempo[i] > 1) {#no se interviene, fuera de la rotación
      print("segunda_condicion")
      N_d[i] <<- N_a[i]
      Dg_d[i] <<- Dg_a[i]
      dist_D_d[[i]] <<- dist_D_a[[i]]
      dist_d_cm_d[[i]] <<- dist_d_cm_a[[i]]
      G_d[i] <<- G_a[i]
      V_d[i] <<- V_a[i]
    } else { #se interviene
      print("tercera_condicion")
      distrib_d_0 <- distribucion_act %>%
        arrange(clase_D) %>% #para comparar el número de árboles entre clases consecutivas
        mutate(ab_exceso = ifelse(N_antes > lead(N_x_y), (N_antes-lead(N_x_y))*pi*(clase_D/200)^2,0)) %>%
        mutate(ab_exceso = ifelse(clase_D == max(clase_D), G_antes, ab_exceso)) #la última clase siempre se corta (esto quizá no encaja con las criterios ProSilva)
      
      distrib_d_1 <- distrib_d_0 %>%
        filter(ab_exceso > 0) %>%
        arrange(desc(clase_D)) %>% #para intervenir de arriba a abajo en diámetro
        mutate(cum_ = cumsum(ab_exceso)) %>%
        mutate(cum_1 = cum_- G_a[i]*peso_G) %>%
        mutate(signo_cum_= sign(cum_1)) %>%
        mutate(diff_ = c(0, diff(signo_cum_))) %>%
        mutate(mult = (signo_cum_ == -1)*ab_exceso + (diff_ == 2)*c(0, cum_1[1:(n()-1)]) + (c(sign(cum_1[[1]]) >=0, rep(0, n()-1)))*cum_1) %>%
        mutate(ab_extr_clase = abs(mult)) %>%
        mutate(n_extraido_clase = ab_extr_clase/G_antes*N_antes) %>%
        mutate(N_despues = N_antes - n_extraido_clase) %>%
        mutate(G_despues = G_antes - ab_extr_clase) %>%
        mutate(V_despues = V_antes/N_antes*N_despues) %>%
        filter(n_extraido_clase > 0) %>%
        select(names(distribucion_act), N_despues, G_despues, V_despues)
      
      distrib_d <- distribucion_act %>%
        filter(!(clase_D %in% distrib_d_1$clase_D)) %>%
        mutate(N_despues = N_antes, G_despues = G_antes, V_despues = V_antes) %>%
        bind_rows(distrib_d_1) %>%
        arrange(clase_D)
      
      df_despues <-  dist_d_cm_a[[i]] %>%
        select(d_, n_d, clase_D) %>%
        left_join(distrib_d %>% select(clase_D, N_despues)) %>%
        group_by(clase_D) %>%
        mutate(n_d = n_d/sum(n_d)*N_despues) %>%
        ungroup() %>%
        select(d_, n_d, clase_D)
      
      # distribucion_act <- distribucion_act %>%
      #   select(-N_antes, -G_antes, -V_antes) %>%
      #   rename(N_antes = N_despues, G_antes = G_despues, G_antes = G_despues)
      
      #actualizar los parámetros dasocráticos tras la corta
      N_d[i] <<- sum(distrib_d$N_despues)
      G_d[i] <<- sum(distrib_d$G_despues)
      Dg_d[i] <<- sqrt(G_d[1]*(40000/(pi*N_d[1])))
      dist_D_d[[i]] <<- distrib_d
      dist_d_cm_d[[i]] <<- df_despues
      V_d[i] <<- sum(distrib_d$V_despues)
      
    }
    
  }
  
 
  

  #se eliminan preferentemente en el estrato dominante y codominante (se empieza de arriba a abajo)
  #se considera dominado al tercio de diámetro inferior (no se usa este criterio de momento)
    # distrib_d_0 <- distribucion_act %>%
    # arrange(desc(clase_D)) %>%
    # mutate(ab_exceso = ifelse(N_antes > N_x_y, G_antes-ab_,0))
    # 
    # distrib_d_1 <- distrib_d_0 %>%
    #   filter(ab_exceso > 0) %>%
    #   mutate(cum_ = cumsum(ab_exceso)) %>%
    #   mutate(cum_1 = cum_- G_a[i]*peso_G) %>%
    #   mutate(signo_cum_= sign(cum_1)) %>%
    #   mutate(diff_ = c(0, diff(signo_cum_))) %>%
    #   mutate(mult = (signo_cum_ == -1)*ab_exceso + (diff_ == 2)*c(0, cum_1[1:(n()-1)]) + (c(sign(cum_1[[1]]) >=0, rep(0, n()-1)))*cum_1) %>%
    #   mutate(ab_extr_clase = abs(mult)) %>%
    #   mutate(n_extraido_clase = ab_extr_clase/G_antes*N_antes) %>%
    #   mutate(N_despues = N_antes - n_extraido_clase) %>%
    #   mutate(G_despues = G_antes - ab_extr_clase) %>%
    #   mutate(V_despues = V_antes/N_antes*N_despues) %>%
    #   filter(N_despues > 0) %>%
    #   select(names(distribucion_act), N_despues, G_despues, V_despues)
    # 
    # distrib_d <- distribucion_act %>%
    #   filter(!(clase_D %in% distrib_d_1$clase_D)) %>%
    #   mutate(N_despues = N_antes, G_despues = G_antes, V_despues = V_antes) %>%
    #   bind_rows(distrib_d_1) %>%
    #   arrange(clase_D)
    #   
  
 
    #funcion para incrementar el crecimiento anual
    funcion_increm_anual <- function(i) {
      #recuperar la distribución que se va a actualizar
      actualiza_0 <- dist_d_cm_d[[i-1]] %>%
        na.omit()

      
      newdata_ = actualiza_0 %>%
        rename(Dn_ifn3 = d_) %>%
        mutate(ab_ = n_d*pi*(Dn_ifn3/200)^2) %>% #cálculo de ab en árboles de más diámetro (ab_may_ifn3)
        mutate(ab_cumsum = cumsum(ab_)) %>% 
        mutate(ab_may_ifn3 = ab_cumsum - n_d*pi*(Dn_ifn3/200)^2) %>%
        mutate(ab_ifn3 = sum(ab_))
      
      actualiza_1 <- actualiza_0 %>%
        mutate(inc_ab_pred = predict(mod.Biandi_nlm, newdata = newdata_)) %>% 
        mutate(inc_ab_corregido = int_corr_ab + slo_corr_ab*inc_ab_pred) %>%
        mutate(D_act = sqrt((inc_ab_corregido+pi*(d_/2)^2)*4/pi))
      
      df_ <- actualiza_1 %>%
        select(D_act, n_d) %>%
        rename(d_ = D_act) %>%
        mutate(clase_D = cut(d_, breaks = breaks_5cm, labels = seq(5,150,by=5))) %>% #labels identifica el diámetro medio de la clase
        mutate(clase_D = as.numeric(as.character(clase_D)))
      
      df_clase <- df_ %>%
        mutate(ab = pi*(d_/200)^2*n_d) %>%
        mutate(vol = predict(lm.vcc, newdata = data.frame(Dn_mm = d_*10))/1000*n_d) %>%
        group_by(clase_D) %>%
        summarise(N_antes = sum(n_d),
                  G_antes = sum(ab),
                  V_antes = sum(vol)) %>%
        ungroup() %>%
        mutate(extra_ = (clase_D >= max(N_x5$clase_D))) %>%
        mutate(suma_extra_N = sum(N_antes*extra_),
               suma_extra_G = sum(G_antes*extra_),
               suma_extra_V = sum(V_antes*extra_)) %>%
        mutate(N_antes = ifelse(clase_D == max(N_x5$clase_D), suma_extra_N, N_antes),
               G_antes = ifelse(clase_D == max(N_x5$clase_D), suma_extra_G, G_antes),
               V_antes = ifelse(clase_D == max(N_x5$clase_D), suma_extra_V, V_antes)) %>%
        filter(clase_D %in% N_x5$clase_D) %>%
        mutate(N_despues = N_antes, G_despues = G_antes, V_despues = V_antes) %>% #para evitar errores cuando no hay intervenciones
        select(-extra_, -suma_extra_N,-suma_extra_G,-suma_extra_V) 
      
      #variables dasocráticas actualizadas
      N_a[i] <<- sum(df_clase$N_antes)
      G_a[i] <<- sum(df_clase$G_antes)
      Dg_a[i] <<- sqrt(G_a[i]*(40000/(pi*N_a[i])))
      dist_D_a[[i]] <<- df_clase
      dist_d_cm_a[[i]] <<- df_
      V_a[i] <<- sum(df_clase$V_antes)
      
    }

################################################################################
################################################################################    
    #actualizar para la simulación
    # 1º actualizar "después en el primer año"
    funcion_actualizar_despues(1)
    
    # 2º actualizar el resto de años
    for (i in c(2:t_fin)) {
    #for (i in c(2:11)) {
      print(i)
      funcion_increm_anual(i)
      funcion_actualizar_despues(i)
    }

    
      
  
  
  