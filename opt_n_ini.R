#obtener la densidad inicial para alcnzar el número de pies/ha que proponer Madrigal a los 20 años


#función para generar los escenarios en xls para CO2Fix y las piezas para montar el archivo .co2
#Mortalidad natural
#cálculo del diámetro medio cuadrático según el modelo de incremento
#es la misma función que genera escenarios de claras, pero se pasan todos los tratamientos a "mortalidad natural"

#library(openxlsx)
library(zoo)
#library(ggnewscale)
library(ParBayesianOptimization)
library(tidyverse)

#volumen mínimo a extraer en cada clara
vol_min = 40
num_turnos <- 1

#datos de claras para el gráfico
res_daso_claras <- read.csv2("datos/claras/res_daso_claras.csv")
res_daso_claras_0 <- res_daso_claras %>%
  pivot_wider(names_from = "var", values_from = "valor")

#datos de ordenaciones para el gráfico
dat_ordenaciones_adultas <- read.csv("resultados/dat_ordenaciones_adultas.csv")

lista_escenario.nombre_ <- c("mortalidad_IS25_alta",
                             "mortalidad_IS22_alta",
                             "mortalidad_IS19_alta",
                             "mortalidad_IS16_alta",
                             "mortalidad_IS13_alta")
#grupo_ = "mortalidad_natural"
funcion_genera_escenarios_mortalidad_obj <- function(grupo_ = "mortalidad_natural", escenario.nombre_ = "mortalidad_natural_IS22_alta", edad_turno_ = 500,
                                                 N_ini_ = 5469) {
  print(paste(grupo_,escenario.nombre_,N_ini_))
  

  # calidad de estación
  posicion <- gregexpr(pattern ='IS',escenario.nombre_)
  IS <- substr(escenario.nombre_, as.numeric(posicion)+2,  as.numeric(posicion)+3)
  IS <- as.numeric(IS)
  
  #esquema de claras
  esquemas_claras <- c("baja","media","alta")
  localiz_esquema <- c(grepl( esquemas_claras[1], escenario.nombre_, fixed = TRUE),
                       grepl( esquemas_claras[2], escenario.nombre_, fixed = TRUE),
                       grepl( esquemas_claras[3], escenario.nombre_, fixed = TRUE))
  
  esquema_clara <- esquemas_claras[localiz_esquema]
  
  # Denominación del escenario
  escenario.nombre <- escenario.nombre_
  grupo <- grupo_ #"go_fagus" "selv_macizo_pirenaico"
  
  #escenario.tratamiento_0 <- read.csv2(paste0("datos/escenarios/",grupo,"/",escenario.nombre, ".csv"), sep = ";")
  
  # densidad inicial
  N_ini = as.numeric(N_ini_)
  
  # rango de edades
  edad_ini <- 1
  edad_fin <- edad_turno_
  
  #Denominación de las claras tratadas con la función de claras mixtas
  tipos_claras <- c("clara por lo bajo","clara mixta","diseminatoria",
                    "aclaratoria 1", "aclaratoria","clara selectiva",
                    "corta preparatoria","corta diseminatoria","entresaca")
  
  #modelo de incremento individual
  load("datos/mod_inc_diam_individual")
  
  #modelo de diámetro mínimo
  load("resultados/mod_diam_min/lm_dim_min_reg")
  
  # Parámetros de inicio
  # parámetros relación Dg ~ f(N, Ho)
  b0 = 54.2762893472351
  b1 = -0.33521433955872
  b2 = 0.46960192588597
  
  # parámetros relación V ~f(Dg, Ho, N)
  b3 = 0.000231164659251752
  b4 = 1.56932077902809
  b5 = 0.97380243087504
  b6 = 0.901458044581267
  
  # parámetros relación Dm ~ f(Dg, N, G)
  a7 = 0.191406636051764
  a8 = -0.000620621044818245
  a10 = 0.0166602839868406
  
  # tasa de mortalidad natural en 5 años
  mort_lustro = 0.02193159
  mort_anual = mort_lustro/5
  # parámetros del modelo de mortalidad SFA
  mort_interc <- 13.0752583
  mort_pend <- -1.9273713
  
  
  
  edad = seq(edad_ini,edad_fin, by = 1)
  Ho = IS*((1-1/exp(0.02*edad))/(1-1/exp(0.02*80)))^1.4823
  
  escenario.tratamiento_0 <- data.frame(edad = edad) %>%
    mutate(tratamiento = "mortalidad natural") %>%
    mutate(codigo ="", perc_extract="",
           perc_extrac_baja="", perc_extrac_alta="")
  escenario.tratamiento <- data.frame(edad = edad) %>%
    left_join(escenario.tratamiento_0)
  
  escenario.tratamiento[is.na(escenario.tratamiento)] <- ""
  
  # Variables "Antes de la clara"
  N_a <- rep(0, length(edad))
  Dg_a <- rep(0, length(edad))
  dist_D_a <-  vector(mode='list', length=length(edad))
  G_a <- rep(0, length(edad))
  V_a <- rep(0, length(edad))
  vi_a <- rep(0, length(edad))
  H_D_a <- rep(0, length(edad))
  IH_a <- rep(0, length(edad))
  
  # Variables "Después de la clara"
  N_d <- rep(0, length(edad))
  Dg_d <- rep(0, length(edad))
  dist_D_d <- vector(mode='list', length=length(edad))
  G_d <- rep(0, length(edad))
  V_d <- rep(0, length(edad))
  vi_d <- rep(0, length(edad))
  H_D_d <- rep(0, length(edad))
  IH_d <- rep(0, length(edad))
  
  
  # Extraido en la clara
  N_e <- rep(0, length(edad))
  Dg_e <- rep(0, length(edad))
  G_e <- rep(0, length(edad))
  V_e <- rep(0, length(edad))
  IH_e <- rep(0, length(edad))
  
  
  
  #identificar el modelo de evolución del diámetro a aplicar
  # NA para el modelo Dg ~ f(N,Ho), 1 para mod incremento individual
  mod_evol_D <- data.frame(id = ifelse(escenario.tratamiento$tratamiento == "", NA,1)) %>%
    fill(id)
  
  
  
  # Proceso
  
  # 1. Iniciar a la densidad inicial
  # 2. Leer el tratamiento correspondiente a la edad
  # 3. Si no hay intervención, aplicar mortalidad y modelo de crecimiento Diam
  # 4. Si hay intervención, seleccionar los árboles sobre los que se interviene
  # 5. Calcular los parámetros de "Extraido en la clara", "Después de la clara", "Crecimiento"
  
  
  # función de mortalidad
  func_mort <- function(N_, D_) {
    #N_extr_nat = N_*(1-mort_lustro) #no está clara que este sea un dato fiable, hay que revisar con las parcelas de claras
    N_extr_nat = N_*(1-mort_anual) #no está clara que este sea un dato fiable, hay que revisar con las parcelas de claras
    N_extr_SFA = exp(mort_interc+log(D_)*mort_pend)
    return(min(c(N_extr_nat,N_extr_SFA)))
    #return(min(c(N_,N_extr_SFA)))
  }
  
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
  # a: parámetro de localización diámetro mínimo, hay que modelizarlo
  # lo iniciamos a 5
  
  
  
  func_expande_Weibull <- function (i_ = 7, Dg_a_, N_a_) {
    Dg_ = Dg_a_
    N_ = N_a_
    G_= Dg_^2*pi*N_/40000     
    Dm_ = Dg_ - exp(a7+a8*N_+a10*G_)
    var_ = Dg_^2 - Dm_^2
    
    # Dg_ = Dg_d[i_-1]
    # N_ = N_d[i_-1]
    # G_= Dg_^2*pi*N_/40000   
    
    # Dm_ = Dg_ - exp(a7+a8*N_+a10*G_)
    # var_ = Dg_^2 - Dm_^2
    # parámetro a
    if (class(dist_D_d[[i_-1]]) != "data.frame") {
      a = 5
    } else {
      
      # newdata_ = data.frame(Ho_ifn3 = Ho[i_-1],
      #                       ab_ifn3 = (Dg_d[i_-1])^2*pi*(N_d[i_-1])/40000,
      #                       Dn_ifn3 = min(dist_D_d[[i_-1]]$d_),
      #                       dgm_ifn3 = Dg_d[i_-1])
      # inc_ <- predict(m.Richards, newdata = newdata_)
      # a = min(dist_D_d[[i_-1]]$d_)+inc_*(edad[i] - edad[i_-1])
      
      newdata_ = data.frame(Dg = Dg_)
      a = as.numeric(predict(lm.diam_min_reg, newdata = newdata_))
      
    }
    
    # parámetro c
    func = function(x){
      var_ -  ((Dm_ - a)^2 / (gamma(1+1/x))^2) *(gamma(1+2/x) - (gamma(1+1/x))^2)
    }
    c = Bisection(f = func, 0.1, 10) #Bisection(f = func,1,4)
    # parámetro b
    b = (Dm_ - a)/gamma(1+1/c)
    return(c(a,b,c))
  }
  
  
  # función de clara por lo bajo
  # func_clara_bajo <- function(i_ = 1) {
  # N_a_ = N_d[i_-1]
  # Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
  # V_a_ = b3*Dg_a_^b4*Ho[i_]^b5*N_a_^b6
  # dist_D_a_ <- NA #no hace falta calcularlo
  # 
  # V_total = V_a_
  # V_extr = extrac_min
  # #V_total =  b3*Dg_^b4*Ho[i]^b5*N_^b6
  # 
  # param_= func_expande_Weibull(i_, Dg_a_, N_a_) #tiene en cuanta la variación de "a", diámetro mínimo
  # df <- data.frame(d_ = seq(round(param_[1]/0.5)*0.5,150)) %>% #150 como diámetro superior, pero deberíamos parametrizarlo
  #   mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))* N_a_) %>%
  #   filter(!is.na(n_d)) %>% #inf cuando el redondeo está por debajo del diámetro inferior (param[1])
  #   #mutate(n_d = floor(n_d)) %>%
  #   mutate(diam2xN = d_^2*n_d) %>% #estimación de la relación diámetro-volumen
  #   mutate(vol_prop = V_total*diam2xN/sum(diam2xN)) %>% #volumen repartido proporcionalmente por diámetro
  #   #arrange(d_) %>% #operaciones para marcar los diámetros por lo bajo hasta V_extr
  #   mutate(cum_ = cumsum(vol_prop)) %>%
  #   mutate(cum_1 = cum_-V_extr) %>%
  #   mutate(signo_cum_= sign(cum_1)) %>%
  #   mutate(diff_ = c(0, diff(signo_cum_))) %>%
  #   #mutate(lag_diff = lead(diff_)) %>%
  #   #mutate(mult = (signo_cum_ == -1)*vol_prop + (diff_ == 2)*lag(cum_1) ) %>%
  #   mutate(mult = (signo_cum_ == -1)*vol_prop + (diff_ == 2)* c(0, cum_1[1:(n()-1)]) + (c(sign(cum_1[[1]]) >=0,rep(0, n()-1)))*cum_1) %>%
  #   mutate(vol_clase = abs(mult)) %>%
  #   mutate(n_extraido_clase = vol_clase/vol_prop*n_d) %>%
  #   mutate(n_despues_clase = n_d - n_extraido_clase) %>%
  #   filter(n_despues_clase > 0) #eliminar las clases sin árboles
  # 
  # 
  # N_d_ = sum(df$n_despues_clase)
  # Dg_d_ = sqrt(sum(df$n_despues_clase*(df$d_)^2)/sum(df$n_despues_clase))
  # #V_d_ = V_total-V_extr
  # #V_d_ = sum(df$vol_clase) + sum((df$vol_clase ==0)*df$vol_prop)
  # V_d_ <- V_total - V_extr
  # 
  # dist_D_d_ <- df %>% select(d_, n_d)
  #   
  # 
  #   return(list(N_a_,Dg_a_,V_a_,dist_D_a_, N_d_,Dg_d_,V_d_,dist_D_d_))
  # }
  
  
  # función de clara mixta
  func_clara_mixta <- function(i_ = 5) {
    
    codigo = escenario.tratamiento$codigo[[i_]]
    perc_extract = escenario.tratamiento$perc_extract[[i_]]
    perc_extrac_baja = escenario.tratamiento$perc_extrac_baja[[i_]]
    perc_extrac_alta = escenario.tratamiento$perc_extrac_alta[[i_]]
    
    #si el tratamiento es un entero, representa pies/ha después de la intervención
    #se pasa a tanto por uno
    if (!is.na(as.integer(codigo))) {
      perc_extract = (N_d[i_-1]-as.integer(codigo))/N_d[i_-1]
      codigo <- "N"
    }
    
    perc_extract = as.numeric(perc_extract)
    perc_extrac_baja = as.numeric(perc_extrac_baja)
    perc_extrac_alta = as.numeric(perc_extrac_alta)
    
    #distribución de diámetros antes de la clara
    N_a_ = N_d[i_-1]
    #Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
    #cálculo de Dg_a a partir del modelo de incremento
    if (class(dist_D_d[[i_-1]]) == "data.frame") {
      newdata_ <- dist_D_d[[i_-1]] %>%
        rename(Dn_ifn3 = d_) %>%
        mutate (Ho_ifn3 = Ho[i_-1],
                ab_ifn3 = (Dg_d[i_-1])^2*pi*(N_d[i_-1])/40000,
                dgm_ifn3 = Dg_d[i_-1])
      inc_ <- predict(m.Richards, newdata = newdata_)
      d_con_inc_ <- newdata_ %>%
        mutate(predicho = Dn_ifn3+inc_) %>%
        summarise(dg_ = sqrt(sum(n_d*predicho^2)/sum(n_d)))
      Dg_a_ = d_con_inc_[[1]]
    } else {
      Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
    }
    V_a_ = b3*Dg_a_^b4*Ho[i_]^b5*N_a_^b6
    G_a_ = Dg_a_^2*pi*N_a_/40000
    dist_D_a_ <- NA #no hace falta calcularlo
    
    param_= func_expande_Weibull(i_, Dg_a_, N_a_) #tiene en cuanta la variación de "a", diámetro mínimo
    df_ <- data.frame(d_ = seq(round(param_[1]/0.5)*0.5,150)) %>% #150 como diámetro superior, pero deberíamos parametrizarlo
      mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))* N_a_) %>%
      filter(!is.na(n_d)) 
    
    
    
    #contabilizar la parte de la clara por lo bajo
    
    if(codigo == "V") {
      if(perc_extract < vol_min/V_a_) { # el mínimo a extraer se fija con vol_min
        perc_extract = vol_min/V_a_
      }
      var_extract = perc_extract*perc_extrac_baja*V_a_
      #var_extract = perc_extract*perc_extrac_baja*V_a_
    } else if (codigo == "G") {
      g_equivalente = vol_min/(V_a_/G_a_) # área basimétrica equivalente al volumen mínimo a extraer
      if(perc_extract < g_equivalente/G_a_) {
        perc_extract = g_equivalente/G_a_
      }
      var_extract = perc_extract*perc_extrac_baja*G_a_
    } else {
      n_equivalente = vol_min/(V_a_/N_a_) # n equivalente al volumen mínimo a extraer
      if(perc_extract < n_equivalente/N_a_) {
        perc_extract = n_equivalente/N_a_
      }
      var_extract = perc_extract*perc_extrac_baja*N_a_
    }
    
    if(var_extract == 0) {
      df_bajo_ <- df_
    } else {
      df_bajo_ <- df_ %>%
        mutate(diam2xN = d_^2*n_d) %>% #estimación de la relación diámetro-volumen, diámetro-área basimétrica
        #mutate(multip = diam2xN/sum(diam2xN)) %>%
        #mutate(multip_2 = multip*G_a_) %>%
        mutate(var_prop = case_when(
          codigo == "V" ~ V_a_*diam2xN/sum(diam2xN), #volumen repartido proporcionalmente por diámetro
          codigo == "G" ~ G_a_*diam2xN/sum(diam2xN), #área basimétrica repartida proporcionalmente por diámetro
          codigo == "N" ~ n_d #n
        )
        ) %>% 
        arrange(d_) %>% #operaciones para marcar los diámetros por lo bajo hasta V_extr
        mutate(cum_ = cumsum(var_prop)) %>%
        mutate(cum_1 = cum_- var_extract) %>%
        mutate(signo_cum_= sign(cum_1)) %>%
        mutate(diff_ = c(0, diff(signo_cum_))) %>%
        #mutate(lag_diff = lead(diff_)) %>%
        #mutate(mult = (signo_cum_ == -1)*vol_prop + (diff_ == 2)*lag(cum_1) ) %>%
        mutate(mult = (signo_cum_ == -1)*var_prop + (diff_ == 2)*c(0, cum_1[1:(n()-1)]) + (c(sign(cum_1[[1]]) >=0, rep(0, n()-1)))*cum_1) %>%
        mutate(vol_clase = abs(mult)) %>%
        mutate(n_extraido_clase = vol_clase/var_prop*n_d) %>%
        mutate(n_despues_clase = n_d - n_extraido_clase) %>%
        filter(n_despues_clase > 0) %>%
        mutate(n_d = n_despues_clase)
    }
    
    
    #contabilizar la parte de la clara por lo alto
    
    
    if(codigo == "V") {
      if(perc_extract < vol_min/V_a_) { # el mínimo a extraer se fija con vol_min
        perc_extract = vol_min/V_a_
      }
      var_extract = perc_extract*perc_extrac_alta*V_a_
      #var_extract = perc_extract*perc_extrac_baja*V_a_
    } else if (codigo == "G") {
      g_equivalente = vol_min/(V_a_/G_a_) # área basimétrica equivalente al volumen mínimo a extraer
      if(perc_extract < g_equivalente/G_a_) {
        perc_extract = g_equivalente/G_a_
      }
      var_extract = perc_extract*perc_extrac_alta*G_a_
    } else {
      n_equivalente = vol_min/(V_a_/N_a_) # n equivalente al volumen mínimo a extraer
      if(perc_extract < n_equivalente/N_a_) {
        perc_extract = n_equivalente/N_a_
      }
      var_extract = perc_extract*perc_extrac_alta*N_a_
    }
    
    df_alto_ <- df_bajo_ %>%
      select(d_, n_d) %>%
      mutate(diam2xN = d_^2*n_d) %>% #estimación de la relación diámetro-volumen, diámetro-área basimétrica
      #mutate(multip = diam2xN/sum(diam2xN)) %>%
      #mutate(multip_2 = multip*G_a_) %>%
      mutate(var_prop = case_when(
        codigo == "V" ~ V_a_*diam2xN/sum(diam2xN), #volumen repartido proporcionalmente por diámetro
        codigo == "G" ~ G_a_*diam2xN/sum(diam2xN), #área basimétrica repartida proporcionalmente por diámetro
        codigo == "N" ~ n_d #n
      )
      ) %>% 
      filter(round(n_d) > 0) %>%
      arrange(desc(d_)) %>%
      mutate(var_extr_x_clase = var_extract*var_prop/sum(var_prop)) %>%
      mutate(n_extraido_clase = var_extr_x_clase/var_prop*n_d) %>%
      mutate(n_despues_clase = n_d - n_extraido_clase) %>%
      filter(n_despues_clase > 0) %>%
      mutate(n_d = n_despues_clase)
    
    
    #Variables dasocráticas tras la clara
    N_d_ = sum(df_alto_$n_despues_clase)
    Dg_d_ = sqrt(sum(df_alto_$n_despues_clase*(df_alto_$d_)^2)/sum(df_alto_$n_despues_clase))
    
    #Volumen después de la clara, calculado como % de los diámetros que han quedado respecto al inicial
    dist_vol_antes <- df_ %>%
      mutate(diam2xN = d_^2*n_d) %>% #estimación de la relación diámetro-volumen
      mutate(vol_prop = V_a_*diam2xN/sum(diam2xN)) %>%
      mutate(vol_prop_indiv = vol_prop/n_d) %>%
      select(d_, vol_prop_indiv)
    
    dis_vol_despues <- df_alto_ %>%
      left_join(dist_vol_antes) %>%
      mutate(vol_desp = vol_prop_indiv*n_despues_clase)
    
    V_d_ <- sum(dis_vol_despues$vol_desp)
    #V_d_ = b3*Dg_d_^b4*Ho[i_]^b5*N_d_^b6
    
    dist_D_d_ <- df_alto_ %>% select(d_, n_d)
    
    
    return(list(N_a_,Dg_a_,V_a_,dist_D_a_, N_d_,Dg_d_,V_d_,dist_D_d_))
  }
  
  # función de tratamiento
  # devuelve N_d
  # si no hay tratamiento, se actualiza con la mortalidad
  # si es "clareo" se actualiza Dg con la fórmula de masa ~ f(N,Ho)
  func_trat <- function(tratam_ = "clareo", i_ = 2) {
    if (tratam_ == "") {
      #antes
      N_a_ <- N_d[i_-1]
      Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
      V_a_ <- b3*Dg_a_^b4*Ho[i_]^b5*N_a_^b6
      if (is.na(mod_evol_D$id[i_])) {
        dist_D_a_ <- NA
      } else  {
        #} else if (is.null(dist_D_d[[i_-1]])) {
        param_= func_expande_Weibull(i_, Dg_a_, N_a_)
        df <- data.frame(d_ = seq(round(param_[1]/0.5)*0.5,150)) %>% #150 como diámetro superior, pero deberíamos parametrizarlo
          mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))* N_a_) %>%
          filter(!is.na(n_d)) %>%
          #mutate(n_d = floor(n_d)) %>%
          filter(n_d > 0)
        dist_D_a_ <- df
      }
      
      #después
      N_d_ =  N_a_ #no hay efecto de la mortalidad, se supone que va incluido en los tratamientos #func_mort(N_a_, Dg_a_)  #func_mort(N_d[i_-1], Dg_d[i_-1])
      Dg_d_ = b0*N_d_^b1*Ho[i_]^b2
      V_d_ <- b3*Dg_d_^b4*Ho[i_]^b5*N_d_^b6
      dist_D_d_ <- dist_D_a_ #esta distribución sólo se usa a efectos del cálculo del diámetro mínimo.
      #se asume que el diámetro mínimo no cambia tras la mortalidad natural, es un fenómeno aleatorio
      # N_d_ = func_mort(N_d[i_-1], Dg_d[i_-1])
      # Dg_d_ = b0*N_d_^b1*Ho[i]^b2
      # V_d_ <- b3*Dg_d_^b4*Ho[i]^b5*N_d_^b6
      
      return(list(N_a_,Dg_a_,V_a_,dist_D_a_, N_d_,Dg_d_,V_d_,dist_D_d_))
      
    } else if (tratam_ == "mortalidad natural") {
      #antes
      N_a_ <- N_d[i_-1]
      Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
      V_a_ <- b3*Dg_a_^b4*Ho[i_]^b5*N_a_^b6
      dist_D_a_ <- NA
      
      # if (is.na(mod_evol_D$id[i_])) {
      #   dist_D_a_ <- NA
      # } else  {
      #   #} else if (is.null(dist_D_d[[i_-1]])) {
      #   param_= func_expande_Weibull(i_, Dg_a_, N_a_)
      #   df <- data.frame(d_ = seq(round(param_[1]/0.5)*0.5,150)) %>% #150 como diámetro superior, pero deberíamos parametrizarlo
      #     mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))* N_a_) %>%
      #     filter(!is.na(n_d)) %>%
      #     #mutate(n_d = floor(n_d)) %>%
      #     filter(n_d > 0)
      #   dist_D_a_ <- df
      # }
      
      #después
      N_d_ = func_mort(N_a_, Dg_a_)
      Dg_d_ = b0*N_d_^b1*Ho[i_]^b2
      V_d_ <- b3*Dg_d_^b4*Ho[i_]^b5*N_d_^b6
      dist_D_d_ <- dist_D_a_ #esta distribución sólo se usa a efectos del cálculo del diámetro mínimo.
      #se asume que el diámetro mínimo no cambia tras la mortalidad natural, es un fenómeno aleatorio
      # N_d_ = func_mort(N_d[i_-1], Dg_d[i_-1])
      # Dg_d_ = b0*N_d_^b1*Ho[i]^b2
      # V_d_ <- b3*Dg_d_^b4*Ho[i]^b5*N_d_^b6
      
      return(list(N_a_,Dg_a_,V_a_,dist_D_a_, N_d_,Dg_d_,V_d_,dist_D_d_))
    } else if (tratam_ == "clareo") {
      clareo <- as.integer(escenario.tratamiento$codigo[[i_]])
      #antes
      N_a_ <- N_d[i_-1]
      Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
      V_a_ <- b3*Dg_a_^b4*Ho[i_]^b5*N_a_^b6
      if (is.na(mod_evol_D$id[i_])) {
        dist_D_a_ <- NA
        #} else if (is.null(dist_D_d[[i_-1]]) | is.na(dist_D_d[[i_-1]])) {
      } else  {
        param_= func_expande_Weibull(i_, Dg_a_, N_a_)
        df <- data.frame(d_ = seq(round(param_[1]/0.5)*0.5,1000)) %>% #100 como diámetro superior, pero deberíamos parametrizarlo
          mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))* N_a_) %>%
          filter(!is.na(n_d)) %>%
          #mutate(n_d = floor(n_d)) %>%
          filter(n_d > 0)
        
        dist_D_a_ <- df
      }
      #después
      N_d_ = clareo
      Dg_d_ = b0*N_d_^b1*Ho[i_]^b2
      V_d_ <- b3*Dg_d_^b4*Ho[i_]^b5*N_d_^b6
      dist_D_d_ <- dist_D_a_ #esta distribución sólo se usa a efectos del cálculo del diámetro mínimo.
      #se asume que el diámetro mínimo no cambia tras el clareo, la selección de árboles es aleatoria o sistemática
      
      return(list(N_a_,Dg_a_,V_a_,dist_D_a_, N_d_,Dg_d_,V_d_,dist_D_d_))
      
      #} else if (tratam_ == "clara por lo bajo") {
    } else if (tratam_ %in% tipos_claras) {
      func_clara_mixta(i_= i_)
    } else if (tratam_ == "final") {
      #antes
      N_a_ <- N_d[i_-1]
      Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
      V_a_ <- b3*Dg_a_^b4*Ho[i_]^b5*N_a_^b6
      dist_D_a_ <- NULL
      #despues
      N_d_ = 0
      Dg_d_ = 0
      V_d_ <- 0
      dist_D_d_ <- NULL
      
      return(list(N_a_,Dg_a_,V_a_,dist_D_a_, N_d_,Dg_d_,V_d_,dist_D_d_))
    }
  }
  
  # 1. Iniciar a la densidad inicial
  
  N_a[1] <- N_ini
  Dg_a[1] <- b0*N_a[1]^b1*Ho[1]^b2
  V_a[1] <- b3*Dg_a[1]^b4*Ho[1]^b5*N_a[1]^b6
  #dist_D_a[1] <- NA
  
  N_d[1] <- N_a[1] 
  Dg_d[1] <- Dg_a[1]
  V_d[1] <- V_a[1]
  #dist_D_d[1] <- NA
  
  N_e[1] <- 0 
  Dg_e[1] <- 0
  V_e[1] <- 0
  
 
  
  # 2. Avance secuencial
  
  i = 2
  for (i in c(2:edad_obj)) { 
    #for (i in c(2:40)) { 
    #N_a[i] <- N_d[i-1]
    print(i)
    avance_ <- func_trat(tratam_ = escenario.tratamiento$tratamiento[i], i_ = i)
    #N_d[i] <- func_trat(tratam = escenario.tratamiento$tratamiento[i], i_ = i)
    N_a[i] <- avance_[[1]]
    Dg_a[i] <- avance_[[2]]
    V_a[i] <- avance_[[3]]
    dist_D_a[[i]] <- avance_[[4]]
    N_d[i] <- avance_[[5]]
    Dg_d[i] <- avance_[[6]]
    V_d[i] <- avance_[[7]]
    dist_D_d[[i]] <- avance_[[8]]
    
      }
  return (N_d[edad_obj])
}

#Ajuste de hiperparámetros

# for reproducibility
set.seed(123)
grupo_0 = "mortalidad_natural"
escenario.nombre_0 = "mortalidad_natural_IS22_alta"
n_arb_obj = 6000
edad_obj = 15

scoringFunction <- function(N_ini_) {
  
  N_result <- funcion_genera_escenarios_mortalidad_obj(grupo_ = grupo_0, escenario.nombre_ = escenario.nombre_0, edad_turno_ = 500,
                                                               N_ini_ = N_ini_) 
  return(list(Score = 1/abs(n_arb_obj-N_result))
         
  )
}

#definir límites de la búsqueda
bounds <- list( 
  N_ini_ = c(n_arb_obj, as.integer(n_arb_obj*(1+0.02)^20))
)

#optimización

initGrid_ <- data.frame(  N_ini_ = c(as.integer(n_arb_obj*1.01),
                                       as.integer(n_arb_obj*1.2),
                                       as.integer(n_arb_obj*1.1)))
#initGrid_ <- data.frame(  perc_int = c(-0.2,-0.1,0,0.1,0.2))

tNoPar <- system.time(
  optObj <- bayesOpt(
    FUN = scoringFunction
    , bounds = bounds
    , initGrid = initGrid_
    #, initPoints = 6
    , iters.n = 20
    , iters.k = 1
  )
)


#resultados
optObj$scoreSummary      
optObj$scoreSummary$N_ini[which.max(optObj$scoreSummary$Score)]

