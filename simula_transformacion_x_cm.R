#código para calcular simular la transformación a monte alto irregular
#a partir de datos iniciales en una edad concreta
#la actualización del crecimiento en diámetro se hace sobre la distribución centimétrica
#se asume que la incorporación a la clase inferior es en el número que da la curva teórica

library(openxlsx)
library(zoo)
library(ggnewscale)
library(tidyverse)

#Grupo
grupo <- "transformacion"

N_ini = 0
#grupo = 0
escenario.nombre = 0

#Denominación de las claras tratadas con la función de claras mixtas
tipos_claras <- c("clara por lo bajo","clara mixta","diseminatoria","aclaratoria 1", "aclaratoria","clara selectiva","corta preparatoria","corta diseminatoria", "entresaca")

#Datos de partida

  #Duración de la simulación
  t_fin = 200

  #Índice de sitio, se asimilan las calidades mala, media, buena... a las de montes regulares
  IS = 19

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
  
  #relación altura ~ diámetro
  load("datos/mod_altura_diametro")
  
  # parámetros relación Dm ~ f(Dg, N, G)
  # se emplea la relación de masas regulares
  a7 = 0.191406636051764
  a8 = -0.000620621044818245
  a10 = 0.0166602839868406
  
  #secuencia de la simulación
  tiempo = seq(1,t_fin)
  
  #tratamiento
  tratamiento <- rep("", length(tiempo))
  
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
  
  # Volumen de carpintería
  V_carp <- rep(0, length(tiempo))
  
  # Relaciones de fracciones de biomasa
  Rel_growth_foliage <- rep(0, length(tiempo))
  Rel_growth_branches <- rep(0, length(tiempo))
  Rel_growth_roots <- rep(0, length(tiempo))
  
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
    
    df_v_carp <- df_ %>%
      mutate(ab = pi*(d_/200)^2*n_d) %>%
      mutate(vol = predict(lm.vcc, newdata = data.frame(Dn_mm = d_*10))/1000*n_d) %>%
      mutate(Ht = predict(d_h_nlm, newdata= data.frame(Dn = d_))) %>%
      arrange(desc(d_)) %>% #cálculo bal, área basimétrica por encima del diámetro del árbol
      mutate(ab_cumsum = cumsum(ab)) %>% #cálculo bal
      mutate(bal = ab_cumsum - n_d*pi*(d_/200)^2) %>%
      mutate(cum_ = cumsum(n_d)) %>% #cálculo Ho, criterio de Assmann
      mutate(cum_1 = cum_-100) %>%
      mutate(signo_cum_= sign(cum_1)) %>%
      mutate(diff_ = c(0, diff(signo_cum_))) %>%
      mutate(mult = (signo_cum_ == -1)*n_d + (diff_ == 2)*cum_1) %>%
      mutate(Ho = sum(mult*Ht)/sum(mult)) %>%
      mutate(IH = 100/Ho*sqrt(20000/(sqrt(3)*sum(n_d)))) %>% #Índice de Hart-Becking
      mutate(Dg = sqrt(sum(n_d*d_^2)/sum(n_d))) %>% #Diámetro medio cuadrático
      #variables de los modelos de madera de carpintería
      mutate(VCC = sum(vol),
             bal_mod_gih = bal/(sum(ab)*IH),
             rddg = d_/Dg,
             N = sum(n_d),
             G = sum(ab)) %>%
      mutate( v_carp_1 = exp(-4.1871492+0.0029901*VCC-0.0178539*bal_mod_gih^2),
              v_carp_2 = exp(-10.1197792+2.0351346*rddg+0.1360856*Dg+0.0012059*N),
              v_carp_3 = exp(-5.2618050+0.0679419*d_+0.0004858*N),
              v_carp_4 = exp(-6.783112002+0.082219560*d_+0.003219738*N-0.000002936*N^2+0.026264302*G)) %>%
      mutate(v_carp = v_carp_1+v_carp_2+v_carp_3+v_carp_4) %>%
      mutate(v_carp = ifelse(v_carp > vol, vol, v_carp)) %>%
      mutate(v_carp = ifelse(d_ >= 20, v_carp, 0)) %>% #sólo se considera para sierra por encima de 20 cm
      #fracciones de biomasa
      mutate(B_hoja = 0.0167*d_^2.951*Ht^-1.101, #Batelink 1997
             B_fuste = exp(0.23272^2/2)*exp(-1.63732)*d_^2.21464,
             B_rama7 = exp(0.62932^2/2)*exp(-10.811)*d_^4.08961,
             B_rama2_7 = exp(0.333796^2/2)*exp(-3.86719)*d_^2.34551,
             B_rama2 = exp(0.425041^2/2)*exp(-2.57396)*d_^1.84345,
             # B_raiz_1 = exp(0.459735^2/2)*exp(-1.72224)*Dg_a_^1.25755, #la del manual, no parece correcta
             B_raiz = 0.106*d_^2) %>% #de la tesis de Ricardo
      mutate(B_rama = B_rama7+B_rama2_7+B_rama2) %>%
      select(d_, n_d, clase_D, ab, vol, v_carp, B_fuste, B_rama, B_hoja, B_raiz)
    
    
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
    dist_d_cm_a[[1]] = df_v_carp
    #dist_d_cm_a[[1]] = df_
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
      
      #actualizar volumen de carpintería y fracciones de biomasa
      df_vcarp_biom <-  dist_d_cm_a[[i]] %>%
        #select(d_, n_d, clase_D) %>%
        left_join(distrib_d %>% select(clase_D, N_antes, N_despues)) %>%
        group_by(clase_D) %>%
        mutate(n_d_despues = n_d/sum(n_d)*N_despues) %>%
        ungroup() %>%
        mutate(n_d_extraido = ifelse(n_d <= n_d_despues, 0, n_d - n_d_despues)) %>%
        mutate(v_carp = v_carp*n_d_extraido) %>%
        mutate(v_extraido = vol*n_d_extraido) %>%
        mutate(v_carp = ifelse(v_carp > v_extraido, v_extraido, v_carp)) %>%
        mutate(v_carp = ifelse(d_ >= 20, v_carp, 0)) %>% #sólo se considera para sierra por encima de 20 cm
        mutate(B_fuste = B_fuste*n_d,
               B_rama = B_rama*n_d,
               B_hoja = B_hoja*n_d,
               B_raiz = B_raiz*n_d) 
        
        # Volumen de carpintería
        V_carp[i] <<- 0
        
        # Relaciones de fracciones de biomasa
        Rel_growth_foliage[i] <<- round(sum(df_vcarp_biom$B_hoja, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
        Rel_growth_branches[i] <<- round(sum(df_vcarp_biom$B_rama, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
        Rel_growth_roots[i] <<- round(sum(df_vcarp_biom$B_raiz, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
      
    } else if (tiempo[i] %% rota != 0  & tiempo[i] > 1) {#no se interviene, fuera de la rotación
      print("segunda_condicion")
      N_d[i] <<- N_a[i]
      Dg_d[i] <<- Dg_a[i]
      dist_D_d[[i]] <<- dist_D_a[[i]]
      dist_d_cm_d[[i]] <<- dist_d_cm_a[[i]]
      G_d[i] <<- G_a[i]
      V_d[i] <<- V_a[i]
      
      #actualizar volumen de carpintería y fracciones de biomasa
      df_vcarp_biom <-  dist_d_cm_a[[i]] %>%
        #select(d_, n_d, clase_D) %>%
        left_join(distrib_d %>% select(clase_D, N_antes, N_despues)) %>%
        group_by(clase_D) %>%
        mutate(n_d_despues = n_d/sum(n_d)*N_despues) %>%
        ungroup() %>%
        mutate(n_d_extraido = ifelse(n_d <= n_d_despues, 0, n_d - n_d_despues)) %>%
        mutate(v_carp = v_carp*n_d_extraido) %>%
        mutate(v_extraido = vol*n_d_extraido) %>%
        mutate(v_carp = ifelse(v_carp > v_extraido, v_extraido, v_carp)) %>%
        mutate(v_carp = ifelse(d_ >= 20, v_carp, 0)) %>% #sólo se considera para sierra por encima de 20 cm
        mutate(B_fuste = B_fuste*n_d,
               B_rama = B_rama*n_d,
               B_hoja = B_hoja*n_d,
               B_raiz = B_raiz*n_d) 
      
      # Volumen de carpintería
      V_carp[i] <<- 0
      
      # Relaciones de fracciones de biomasa
      Rel_growth_foliage[i] <<- round(sum(df_vcarp_biom$B_hoja, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
      Rel_growth_branches[i] <<- round(sum(df_vcarp_biom$B_rama, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
      Rel_growth_roots[i] <<- round(sum(df_vcarp_biom$B_raiz, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
      
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
      
      df_vcarp_biom <-  dist_d_cm_a[[i]] %>%
        #select(d_, n_d, clase_D) %>%
        left_join(distrib_d %>% select(clase_D, N_antes, N_despues)) %>%
        group_by(clase_D) %>%
        mutate(n_d_despues = n_d/sum(n_d)*N_despues) %>%
        ungroup() %>%
        mutate(n_d_extraido = ifelse(n_d <= n_d_despues, 0, n_d - n_d_despues)) %>%
        mutate(v_carp = v_carp*n_d_extraido) %>%
        mutate(v_extraido = vol*n_d_extraido) %>%
        mutate(v_carp = ifelse(v_carp > v_extraido, v_extraido, v_carp)) %>%
        mutate(v_carp = ifelse(d_ >= 20, v_carp, 0)) %>% #sólo se considera para sierra por encima de 20 cm
        mutate(B_fuste = B_fuste*n_d,
               B_rama = B_rama*n_d,
               B_hoja = B_hoja*n_d,
               B_raiz = B_raiz*n_d) 
      
      df_despues <-  dist_d_cm_a[[i]] %>%
        select(d_, n_d, clase_D) %>%
        left_join(distrib_d %>% select(clase_D, N_despues)) %>%
        group_by(clase_D) %>%
        mutate(n_d = n_d/sum(n_d)*N_despues) %>%
        ungroup() %>%
        select(d_, n_d, clase_D)
      
      #actualizar los parámetros dasocráticos tras la corta
      tratamiento[i] <<- "entresaca"
      N_d[i] <<- sum(distrib_d$N_despues)
      G_d[i] <<- sum(distrib_d$G_despues)
      Dg_d[i] <<- sqrt(G_d[1]*(40000/(pi*N_d[1])))
      dist_D_d[[i]] <<- distrib_d
      dist_d_cm_d[[i]] <<- df_despues
      V_d[i] <<- sum(distrib_d$V_despues)
      
      # Volumen de carpintería
      V_carp[i] <<- sum(df_vcarp_biom$v_carp_ext, na.rm = TRUE)
      
      # Relaciones de fracciones de biomasa
      Rel_growth_foliage[i] <<- round(sum(df_vcarp_biom$B_hoja, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
      Rel_growth_branches[i] <<- round(sum(df_vcarp_biom$B_rama, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
      Rel_growth_roots[i] <<- round(sum(df_vcarp_biom$B_raiz, na.rm = TRUE)/sum(df_vcarp_biom$B_fuste, na.rm = TRUE),3)
      
    }
    
  }
  

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
      
      #se asegura que se incorporan a la clase inferior para alcanzar el número de árboles de la curva teórica en esa clase
      #se añaden los árboles necesarios al diámetro inferior, 7,5 cm
      sum_cla_inferior = sum(df_$n_d[df_$clase_D == min(N_x5$clase_D)]) #árboles en la clase inferior
      teor_cla_inferior =   N_x5$N_x_y[which.min(N_x5$clase_D)] #árboles en la clase inferior según la curva objetivo
      
      if (sum_cla_inferior < teor_cla_inferior) {
        df_clas_min <- data.frame(d_ = min(N_x5$diam_min_),
                                   n_d = teor_cla_inferior - sum_cla_inferior,
                                   clase_D = min(N_x5$clase_D)) 
        df_ <- df_ %>%
          filter(clase_D >  min(N_x5$diam_min_)) %>%
          bind_rows(df_clas_min)
      }
       
      #hay que calcular el volumen de carpintería mediante ecuaciones de árbol
      #las que se emplean en montes regulares son de masa, pero no parece correcto aplicarlas
      #a montes irregulares
      #Lo mismo para los datos de fracciones de biomasa
      df_v_carp <- df_ %>%
        mutate(ab = pi*(d_/200)^2*n_d) %>%
        mutate(vol = predict(lm.vcc, newdata = data.frame(Dn_mm = d_*10))/1000*n_d) %>%
        mutate(Ht = predict(d_h_nlm, newdata= data.frame(Dn = d_))) %>%
        arrange(desc(d_)) %>% #cálculo bal, área basimétrica por encima del diámetro del árbol
        mutate(ab_cumsum = cumsum(ab)) %>% #cálculo bal
        mutate(bal = ab_cumsum - n_d*pi*(d_/200)^2) %>%
        mutate(cum_ = cumsum(n_d)) %>% #cálculo Ho, criterio de Assmann
        mutate(cum_1 = cum_-100) %>%
        mutate(signo_cum_= sign(cum_1)) %>%
        mutate(diff_ = c(0, diff(signo_cum_))) %>%
        mutate(mult = (signo_cum_ == -1)*n_d + (diff_ == 2)*cum_1) %>%
        mutate(Ho = sum(mult*Ht)/sum(mult)) %>%
        mutate(IH = 100/Ho*sqrt(20000/(sqrt(3)*sum(n_d)))) %>% #Índice de Hart-Becking
        mutate(Dg = sqrt(sum(n_d*d_^2)/sum(n_d))) %>% #Diámetro medio cuadrático
        #variables de los modelos de madera de carpintería
        mutate(VCC = sum(vol),
               bal_mod_gih = bal/(sum(ab)*IH),
               rddg = d_/Dg,
               N = sum(n_d),
               G = sum(ab)) %>%
        mutate( v_carp_1 = exp(-4.1871492+0.0029901*VCC-0.0178539*bal_mod_gih^2),
                v_carp_2 = exp(-10.1197792+2.0351346*rddg+0.1360856*Dg+0.0012059*N),
                v_carp_3 = exp(-5.2618050+0.0679419*d_+0.0004858*N),
                v_carp_4 = exp(-6.783112002+0.082219560*d_+0.003219738*N-0.000002936*N^2+0.026264302*G)) %>%
        mutate(v_carp = v_carp_1+v_carp_2+v_carp_3+v_carp_4)

        #fracciones de biomasa
        mutate(B_hoja = 0.0167*d_^2.951*Ht^-1.101, #Batelink 1997
               B_fuste = exp(0.23272^2/2)*exp(-1.63732)*d_^2.21464,
               B_rama7 = exp(0.62932^2/2)*exp(-10.811)*d_^4.08961,
               B_rama2_7 = exp(0.333796^2/2)*exp(-3.86719)*d_^2.34551,
               B_rama2 = exp(0.425041^2/2)*exp(-2.57396)*d_^1.84345,
               # B_raiz_1 = exp(0.459735^2/2)*exp(-1.72224)*Dg_a_^1.25755, #la del manual, no parece correcta
               B_raiz = 0.106*d_^2) %>% #de la tesis de Ricardo
        mutate(B_rama = B_rama7+B_rama2_7+B_rama2) %>%
        select(d_, n_d, clase_D, ab, vol, v_carp, B_fuste, B_rama, B_hoja, B_raiz)
        
      df_clase <- df_v_carp %>%
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
      
      #en caso de que alguna clase diamétrica esté vacía se incluye con valores de N,G y V a cero, para que no genere NA posteriormente
      if (!identical(df_clase$clase_D,N_x5$clase_D) ) {
        clase_D_falta = N_x5$clase_D[!(N_x5$clase_D %in% df_clase$clase_D)]
        clase_incluir = data.frame(clase_D = clase_D_falta, N_antes=0, G_antes=0, V_antes=0,
                                   N_despues=0, G_despues=0, V_despues=0)
        df_clase <- df_clase %>%
          bind_rows(clase_incluir)
        
      }
      
      #variables dasocráticas actualizadas
      N_a[i] <<- sum(df_clase$N_antes)
      G_a[i] <<- sum(df_clase$G_antes)
      Dg_a[i] <<- sqrt(G_a[i]*(40000/(pi*N_a[i])))
      dist_D_a[[i]] <<- df_clase
      #dist_d_cm_a[[i]] <<- df_
      dist_d_cm_a[[i]] <<- df_v_carp
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

    
    res <- data.frame(IS_ = IS, Tiempo = tiempo,
                      N_a_ = N_a, G_a_ = G_a, Dg_a_ = Dg_a, V_a_ = V_a,
                      N_d_ = N_d,  G_d_ = G_d, Dg_d_ = Dg_d, V_d_ = V_d,
                      tratamiento = tratamiento) %>%
      mutate(N_e_ = N_a_ - N_d_, G_e_ = G_a_- G_d_) %>%
      mutate(Dg_e = sqrt((G_e_*40000)/(pi*N_e_))) %>%
      mutate(V_e_ = V_a_- V_d_ ) %>%
      mutate(V_carp_ = V_carp) %>% 
      mutate(acum_V_e_ = cumsum(V_e_)) %>%
      mutate(lag_acum = c(0, lag(acum_V_e_)[2:n()])) %>%
      mutate(Crec_Vt_ = V_a_+lag_acum) %>%
      mutate(diff_Crec_Vt = c(0, diff(Crec_Vt_))) %>%
      mutate(Crec_medio_ = Crec_Vt_/Tiempo) %>%
      mutate(Crec_corr_ = diff_Crec_Vt/c(0,diff(Tiempo))) %>%
      mutate(Rel_growth_foliage_ = Rel_growth_foliage) %>%
      mutate(Rel_growth_branches_ = Rel_growth_branches) %>%
      mutate(Rel_growth_roots_ = Rel_growth_roots)
      

    ggplot(res, aes(x=Tiempo, y = Crec_corr_))+geom_point()
    
    #Suavizar el dato de Crec_corr_
    # library(splines)
    # 
    # m.Crec_corr <-lm(res$Crec_corr_~bs(res$Edad ))
    #res$Crec_corr_ <- c(0, predict(m.Crec_corr))
    
    res$Crec_corr_ <- rollmedian(res$Crec_corr_, 5, fill = "extend")
    res$Crec_corr <-  res$Crec_corr_
    
    ggplot(res, aes(x=Tiempo, y = Crec_corr_))+geom_point()+
      geom_line( aes(x=Tiempo, y = Crec_medio_))
    
    #Para eliminar inf, -inf y NA se pasan todo a NA y luego a cero
    # res <- res %>% mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
    #   mutate_if(is.numeric, list(~na_if(., -Inf)))
    
    res[is.na(res)] <- 0
    
    nombres_exc <- c("IS_", "Tiempo", "N_a_", "Dg_a_", "G_a_", "V_a_",
                     "N_e_", "Dg_e", "G_e_", "V_e_", "V_carp_",
                     "N_d_", "Dg_d_", "G_d_", "V_d_",
                     "Crec_Vt_", "Crec_medio_", "Crec_corr_",
                     "Rel_growth_foliage_", "Rel_growth_branches_", "Rel_growth_roots_",
                     "tratamiento")

    para_excel_res <- res[nombres_exc] %>%
      mutate(across(is.numeric, round, digits=3))
    write.xlsx(para_excel_res, paste0("resultados/simulaciones/",grupo,"/",escenario.nombre,"_IS_",IS,".xlsx"))
    
    #gráfica de comparación con las ordenaciones
    #gráficos con ordenaciones y claras
    
    
    graf_mort_ordenaciones_claras <- para_excel_res %>%
      bind_rows(dat_ordenaciones_adultas %>% mutate(origen = "ordenaciones") %>% rename(Tiempo = Edad)) %>%
      bind_rows(res_daso_claras_0  %>% mutate(origen = "claras")%>% rename(Tiempo = Edad)) 
     
    
 
    ggplot(graf_mort_ordenaciones_claras %>% filter(is.na(origen)), aes(x=Tiempo, y=V_a_, col= as.factor(IS_)))+
      geom_line(size = 1)+
      geom_point(data=graf_mort_ordenaciones_claras %>% filter(origen == "ordenaciones"),
                 aes(x= Tiempo, y = V_a_ ), size = 2, color = "blue")+
      new_scale("shape") +
      geom_point(data=graf_mort_ordenaciones_claras %>% filter(origen == "claras"),
                 aes(x= Tiempo, y = V_a_, shape = trat), size = 3,  col = "red")+
      ggtitle(paste0("Datos de ordenaciones. ",grupo,"/",escenario.nombre,"_IS_",IS),
              subtitle = paste0("Tipo: Adultas susceptibles de claras. Evolución desde ", N_ini, " arb/ha"))+
      theme_light()+
      labs(x = "Edad", y = "Vcc m3/ha")+
      labs(color = "Evolución según IS")+
      labs(shape = "Clara. Tratamiento")+
      theme(text = element_text(size = 30))
    
    
    ggsave(paste0("resultados/simulaciones/",grupo,"/",escenario.nombre,"_IS_",IS,"_N_ini_",N_ini,".png"), width = 677.4 , height = 364.416, units = "mm")
    

    para_excel_solo_trat <- para_excel_res %>%
      filter(Tiempo %in% seq(5,500, by=5) | tratamiento %in% c("final","clareo",tipos_claras)) %>%
      mutate(Stems = ifelse(Tiempo %in% seq(5,500, by=5), 1, 0)) %>%
      mutate(CAI = Stems*Crec_corr_) %>%
      mutate(Thinning_Harvest = ifelse(tratamiento %in% c("final","clareo",tipos_claras),1,0)) %>%
      mutate(Fraction_removed = Thinning_Harvest*round(V_e_/V_a_,2)) %>%
      mutate(Stems_log_wood = ifelse(V_carp_ == 0, 0,round(V_carp_/V_e_,3))) %>%
      mutate(Stems_pulp_pap = 1-Stems_log_wood) 
    
    write.xlsx(para_excel_solo_trat, paste0("resultados/simulaciones/",grupo,"/",escenario.nombre,"_IS_",IS,"_resumido.xlsx"))
    
    #resumen para CO2Fix
    para_CO2fix <- para_excel_solo_trat %>%
      select(Tiempo,N_a_,Dg_a_,tratamiento,Stems,CAI, Rel_growth_foliage_, Rel_growth_branches_, Rel_growth_roots_,
             Thinning_Harvest,Fraction_removed,Stems_log_wood, Stems_pulp_pap) 
    write.xlsx(para_CO2fix, paste0("resultados/simulaciones/",grupo,"/",escenario.nombre,"_IS_",IS,"_CO2Fix.xlsx"))
    
    #Resumen del escenario
    retorno <- data.frame(grupo = grupo, escenario = escenario.nombre,
                          N_ini = N_ini, IS = IS, Rotacion = edad_fin,
                          Suma_Crec_corriente = round(sum(res$Crec_corr,na.rm = TRUE),0),
                          Suma_Vol_extraido = round(sum(para_excel_solo_trat$V_e_, na.rm = TRUE),0),
                          Suma_Vol_carpinteria = round(sum(para_excel_solo_trat$V_carp),0))     
  
  
  