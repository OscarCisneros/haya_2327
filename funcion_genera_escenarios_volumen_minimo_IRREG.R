#función para generar los escenarios en xls para CO2Fix y obtener la suma de Crecimiento corriente anual como indicador de calidad del escenario
#se establece un volumen mínimo para rentabilizar las claras. En principio 40 m3/ha, excepto para IS= 13, vol_mi = 30 m3 (da error en algunos escenarios porque no hay suficiente volumen al final)
#Simulación de clara
#cálculo del diámetro medio cuadrático según el modelo de incremento

library(openxlsx)
library(zoo)
library(ggnewscale)
library(tidyverse)

#volumen mínimo a extraer en cada clara
vol_min = 40
num_turnos <- 3

#tipos de masa de inicio, según la ordnación de Aralar

# tipos_masa <- data.frame(tipo_m = c("A","B", "C","D"),
#                          Edad = c(50,30,100,70))

# tipos_masa <- data.frame(tipo_m = c("B"),
#                          Edad = c(30))

#H2 IS25
# tipos_masa <- data.frame(tipo_m = c("A","B", "C","D"),
#                          Edad = c(50,30,100,70))
# tipos_masa <- data.frame(tipo_m = c("A", "C","D"),
#                          Edad = c(50,100,70))
# tipos_masa <- data.frame(tipo_m = c("B"),
#                          Edad = c(30))


#H2 IS22
# tipos_masa <- data.frame(tipo_m = c("A","B", "C","D"),
#                          Edad = c(65,40,120,100))
# tipos_masa <- data.frame(tipo_m = c("A", "C","D"),
#                          Edad = c(65,120,100))
# tipos_masa <- data.frame(tipo_m = c("B"),
#                          Edad = c(40))

#H2 IS19
# tipos_masa <- data.frame(tipo_m = c("A","B", "C","D"),
#                          Edad = c(70,50,170,150))
# tipos_masa <- data.frame(tipo_m = c("A", "C","D"),
#                          Edad = c(65,120,100))
# tipos_masa <- data.frame(tipo_m = c("A","B","D"),
#                          Edad = c(70,50,150))
# tipos_masa <- data.frame(tipo_m = c("C"),
#                          Edad = c(170))

#H2 IS16
# tipos_masa <- data.frame(tipo_m = c("A","B", "C","D"),
#                          Edad = c(85,60,180,160))
# tipos_masa <- data.frame(tipo_m = c("C"),
#                          Edad = c(185))
# tipos_masa <- data.frame(tipo_m = c("A","B","D"),
#                          Edad = c(85,60,160))
# tipos_masa <- data.frame(tipo_m = c("C"),
#                          Edad = c(180))

#H2 IS13
# tipos_masa <- data.frame(tipo_m = c("A","B", "C","D"),
#                          Edad = c(95,70,185,165))

#las calidades 13 y 16 dan error en el tipo B (edad = 30)
# tipos_masa <- data.frame(tipo_m = c("A", "C","D"),
#                          Edad = c(50,100,70))
# tipos_masa <- data.frame(tipo_m = c("C", "D"),
#                          Edad = c(100, 70))
# tipos_masa <- data.frame(tipo_m = c("A","B","D"),
#                          Edad = c(95,70,165))
tipos_masa <- data.frame(tipo_m = c("C"),
                         Edad = c(185))

#datos de claras para el gráfico
res_daso_claras <- read.csv2("datos/claras/res_daso_claras.csv")
res_daso_claras_0 <- res_daso_claras %>%
  pivot_wider(names_from = "var", values_from = "valor")

#datos de ordenaciones para el gráfico
dat_ordenaciones_adultas <- read.csv("resultados/dat_ordenaciones_adultas.csv")

#relación entre volumen de fuste y de tronquillo, Madrigal et al (1992)
rel_v_tronquillo <- read.csv2("datos/rel_v_tronquillo.csv")

# funcion_genera_escenarios_vol_min_IRREG <- function(grupo_ = "prueba", escenario.nombre_ = "plan_comarcal_1_1_8_17_dif_dens_IS13_alta",
#                                                     N_ini_ = 7000)

funcion_genera_escenarios_vol_min_IRREG <- function(grupo_ = "go_fagus", escenario.nombre_ = "H2_IS25_alta",
                                      N_ini_ = 10000) {
print(paste(grupo_,escenario.nombre_,N_ini_))
  
#comprobar si existe directorio para el escenario y en caso contrario crearlo
  ruta_dir_escenario = paste0("resultados/simulaciones/",grupo_,"/",escenario.nombre_,"_",N_ini_)
  if(!file.exists(ruta_dir_escenario)) {
    dir.create(ruta_dir_escenario)
  }
    # calidad de estación
    posicion <- gregexpr(pattern ='IS',escenario.nombre_)
    IS <- substr(escenario.nombre_, as.numeric(posicion)+2,  as.numeric(posicion)+3)
    IS <- as.numeric(IS)
    
    if (IS == 13) {
      vol_min = 30
    }
    
    #esquema de claras
    esquemas_claras <- c("baja","media","alta")
    localiz_esquema <- c(grepl( esquemas_claras[1], escenario.nombre_, fixed = TRUE),
                         grepl( esquemas_claras[2], escenario.nombre_, fixed = TRUE),
                         grepl( esquemas_claras[3], escenario.nombre_, fixed = TRUE))
    
    esquema_clara <- esquemas_claras[localiz_esquema]
    
    # Denominación del escenario
    escenario.nombre <- escenario.nombre_
    grupo <- grupo_ #"go_fagus" "selv_macizo_pirenaico"
    
    escenario.tratamiento_0 <- read.csv2(paste0("datos/escenarios/",grupo,"/",escenario.nombre, ".csv"), sep = ";")
    
    
    # densidad inicial
    N_ini = as.numeric(N_ini_)

    
    # rango de edades
    edad_ini <- 1
    edad_fin <- max(escenario.tratamiento_0$edad)
    
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
    for (i in c(2:nrow(escenario.tratamiento))) { 
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
    
    res <- data.frame(IS_ = IS, Edad = edad, Ho_ = Ho,
                      N_a_ = N_a, Dg_a_ = Dg_a, V_a_ = V_a,
                      N_d_ = N_d, Dg_d_ = Dg_d, V_d_ = V_d,
                      tratamiento = escenario.tratamiento$tratamiento) %>%
      mutate(G_a_ = Dg_a_^2*pi*N_a_/40000, Vi_a_ = V_a_/N_a_, H_D_a_ = Ho_/Dg_a_*100, IH_a_ = 100*(sqrt(20000/(N_a_*(sqrt(3))))/Ho_)) %>%
      mutate(G_d_ = Dg_d_^2*pi*N_d_/40000, H_D_d_ = Ho_/Dg_d_*100, IH_d_ = 100*(sqrt(20000/(N_d_*(sqrt(3))))/Ho_)) %>%
      mutate(N_e_ = N_a_ - N_d_, G_e_ = G_a_- G_d_, IH_e_ = IH_d_ - IH_a_) %>%
      mutate(Dg_e = sqrt((G_e_*40000)/(pi*N_e_))) %>%
      mutate(V_e_ = V_a_- V_d_ ) %>%
      mutate(acum_V_e_ = cumsum(V_e_)) %>%
      mutate(lag_acum = c(0, lag(acum_V_e_)[2:n()])) %>%
      mutate(Crec_Vt_ = V_a_+lag_acum) %>%
      mutate(diff_Crec_Vt = c(0, diff(Crec_Vt_))) %>%
      mutate(Crec_medio_ = Crec_Vt_/Edad) %>%
      mutate(Crec_corr_ = diff_Crec_Vt/c(0,diff(Edad)))
    
    #Suavizar el dato de Crec_corr_
    # library(splines)
    # 
    # m.Crec_corr <-lm(res$Crec_corr_~bs(res$Edad ))
    #res$Crec_corr_ <- c(0, predict(m.Crec_corr))
    
    res$Crec_corr_ <- rollmedian(res$Crec_corr_, 5, fill = "extend")
    res$Crec_corr <-  res$Crec_corr_
    
    
    #Para eliminar inf, -inf y NA se pasan todo a NA y luego a cero
    res <- res %>% mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
      mutate_if(is.numeric, list(~na_if(., -Inf)))
    
    res[is.na(res)] <- 0
    
    
    #extender el número de turnos reflejados
    #se repiten todos los datos excepto la edad, que incrementa
   
    if (num_turnos > 1) {
      res_extendido <- res
      for(i in seq(1,num_turnos-1)) {
        res_extendido <- res_extendido %>%
          bind_rows(res_extendido %>% mutate(Edad = Edad+max(res_extendido$Edad)))
      }
      res <- res_extendido
    }
    
    
    nombres_exc <- c("IS_", "Edad", "Ho_", "N_a_" , "Dg_a_", "G_a_", "V_a_", "Vi_a_", "H_D_a_", "IH_a_",
                     "N_e_", "Dg_e", "G_e_", "IH_e_", "V_e_",
                     "N_d_", "Dg_d_", "G_d_", "V_d_", "H_D_d_", "IH_d_",
                     "Crec_Vt_", "Crec_medio_", "Crec_corr_",
                     "tratamiento")
    para_excel_res <- res[nombres_exc] %>%
      mutate(across(is.numeric, round, digits=1))
    
    write.xlsx(para_excel_res, paste0(ruta_dir_escenario,"/",escenario.nombre,"_IS_",IS,"_",N_ini_,".xlsx"))
    
    #gráfica de comparación con las ordenaciones
    #gráficos con ordenaciones y claras
    
    
    graf_mort_ordenaciones_claras <- para_excel_res %>%
      bind_rows(dat_ordenaciones_adultas %>% mutate(origen = "ordenaciones")) %>%
      bind_rows(res_daso_claras_0  %>% mutate(origen = "claras"))
    
    ggplot(graf_mort_ordenaciones_claras %>% filter(is.na(origen)), aes(x=Edad, y=V_a_, col= as.factor(IS_)))+
      geom_line(size = 4, color = "darkorange")+
      geom_point(data=graf_mort_ordenaciones_claras %>% filter(origen == "ordenaciones"),
                 aes(x= Edad, y = V_a_ ), size = 2, color = "blue")+
      new_scale("shape") +
      geom_point(data=graf_mort_ordenaciones_claras %>% filter(origen == "claras"),
                 aes(x= Edad, y = V_a_, shape = trat), size = 8,  col = "red")+
      ggtitle(paste(paste0("Grupo: ",grupo),  paste0("Escenario: ",escenario.nombre), sep="\n"),
              subtitle = paste0("Línea: Evolución de Vcc m3/ha. Puntos azules: Datos de ordenaciones. Inicio: ", N_ini, " arb/ha"))+
      theme_light()+
      labs(x = "Edad", y = "Vcc m3/ha")+
      labs(color = "Evolución según IS")+
      labs(shape = "Clara. Tratamiento")+
      theme(text = element_text(size = 40))+
      theme(axis.title.y = element_text(size=50, margin = margin(t = 0, r = 20, b = 0, l = 0))) +
      theme(axis.title.x = element_text(size=50, margin = margin(t = 20, r = 0, b = 0, l = 0))) +
      theme(plot.title = element_text(size=60, family = "sans", margin=margin(0,0,30,0)))+
      theme(plot.subtitle=element_text(size=40, face="italic", margin=margin(0,0,20,0)))
    
    
    # ggplot(graf_mort_ordenaciones_claras %>% filter(is.na(origen)), aes(x=Edad, y=V_a_, col= as.factor(IS_)))+
    #   geom_line(size = 1)+
    #   geom_point(data=graf_mort_ordenaciones_claras %>% filter(origen == "ordenaciones"),
    #              aes(x= Edad, y = V_a_ ), size = 2, color = "blue")+
    #   new_scale("shape") +
    #   geom_point(data=graf_mort_ordenaciones_claras %>% filter(origen == "claras"),
    #              aes(x= Edad, y = V_a_, shape = trat), size = 3,  col = "red")+
    #   ggtitle(paste0("Datos de ordenaciones. ",grupo,"/",escenario.nombre,"_IS_",IS),
    #           subtitle = paste0("Tipo: Adultas susceptibles de claras. Evolución desde ", N_ini, " arb/ha"))+
    #   theme_light()+
    #   labs(x = "Edad", y = "Vcc m3/ha")+
    #   labs(color = "Evolución según IS")+
    #   labs(shape = "Clara. Tratamiento")+
    #   theme(text = element_text(size = 30))
    
    
    ggsave(paste0(ruta_dir_escenario,"/",escenario.nombre,"_IS_",IS,"_N_ini_",N_ini,".png"), width = 677.4 , height = 364.416, units = "mm")
    
    # ggplot(graf_mort_ordenaciones_claras %>% filter(is.na(origen)), aes(x=Edad, y=G_a_, col= as.factor(IS_)))+
    #   geom_line(size = 1)+
    #   geom_point(data=graf_mort_ordenaciones_claras %>% filter(origen == "ordenaciones"),
    #              aes(x= Edad, y = G_a_ ), size = 2, color = "blue")+
    #   new_scale("shape") +
    #   geom_point(data=graf_mort_ordenaciones_claras %>% filter(origen == "claras"),
    #              aes(x= Edad, y = G_a_, shape = trat), size = 3,  col = "red")+
    #   ggtitle("Datos de ordenaciones", subtitle = "Tipo: Adultas susceptibles de claras. Evolución desde 3000 arb/ha")+
    #   #scale_x_continuous(breaks = seq(0, 120, by = 5))+
    #   theme_light()+
    #   labs(x = "Edad", y = "AB m2/ha")+
    #   labs(color = "Evolución según IS")+
    #   labs(shape = "Clara. Tratamiento")+
    #   theme(text = element_text(size = 15)) 
    # 
    
    
    para_excel_solo_trat <- para_excel_res %>%
      filter(Edad %in% seq(5,500, by=5) | tratamiento %in% c("mortalidad natural", "final","clareo",tipos_claras)) %>%
      mutate(Mortalidad_natural = ifelse(tratamiento %in% c("mortalidad natural"),1,0)) %>%
      mutate(Mortality = ifelse(V_a_== 0, 0, Mortalidad_natural*round(V_e_/V_a_,4))) %>%
      mutate(Stems = ifelse(Edad %in% seq(5,500, by=5), 1, 0)) %>%
      mutate(CAI = Stems*Crec_corr_) %>%
      mutate(Thinning_Harvest = ifelse(tratamiento %in% c("final","clareo",tipos_claras),1,0)) %>%
      mutate(Fraction_removed = Thinning_Harvest*round(V_e_/V_a_,2)) %>%
      mutate(B_hoja = 0.0167*Dg_a_^2.951*Ho_^-1.101, #Baterlink 1997
             B_fuste = exp(0.23272^2/2)*exp(-1.63732)*Dg_a_^2.21464,
             B_rama7 = exp(0.62932^2/2)*exp(-10.811)*Dg_a_^4.08961,
             B_rama2_7 = exp(0.333796^2/2)*exp(-3.86719)*Dg_a_^2.34551,
             B_rama2 = exp(0.425041^2/2)*exp(-2.57396)*Dg_a_^1.84345,
             # B_raiz_1 = exp(0.459735^2/2)*exp(-1.72224)*Dg_a_^1.25755, #la del manual, no parece correcta
             B_raiz = 0.106*Dg_a_^2 #de la tesis de Ricardo
      ) %>%
      mutate(B_rama = B_rama7+B_rama2_7+B_rama2) %>%
      mutate(Rel_growth_foliage = round(B_hoja/B_fuste,3),
             Rel_growth_branches = round(B_rama/B_fuste,3),
             Rel_growth_roots = round(B_raiz/B_fuste,3)) %>%
      mutate(v_carp_1 = V_e_/(1+exp(5.77198-0.05681*G_a_)), #volumen de carpintería
             v_carp_2 = V_e_/(1+exp(5.2144536-0.0015091*N_a_-0.0855058*Dg_a_+0.0376535*Ho_)),
             v_carp_3 = V_e_/(1+exp(2.0558913+0.0712104*Ho_-0.0003358*Dg_a_^2-0.0233621*G_a_)),
             v_carp_4 = V_e_/(1+exp(2.1768173+0.0020481*N_a_-0.0452115*G_a_+0.0579457*Ho_))) %>%
      mutate(V_carp = v_carp_1+v_carp_2+v_carp_3+v_carp_4) %>%
      mutate(V_carp = ifelse(V_carp >= V_e_, V_e_, V_carp)) %>%
      mutate(V_carp = ifelse(Dg_a_ >= 20, V_carp, 0)) %>% #sólo se considera para sierra por encima de 20 cm
      mutate(etiqueta_tronq = cut(Dg_a_, breaks =  rel_v_tronquillo$clas_inf, labels = rel_v_tronquillo$etiqueta_tronq[1:9])) %>%
      mutate(etiqueta_tronq = as.numeric(as.character(etiqueta_tronq))) %>%
      left_join(rel_v_tronquillo %>% select(etiqueta_tronq, v_tronquillo)) %>%
      mutate(Stems_log_wood = (100-v_tronquillo)/100) %>% #Madrigal et al. (1992)
      mutate(Stems_pulp_pap = 1-Stems_log_wood) %>%
      # mutate(Stems_log_wood = 0.75) %>% #clasificaciones válidas MEF/UNE (manual Gofagus, pp. 121)
      # mutate(Stems_pulp_pap = 1-Stems_log_wood) %>%
      # mutate(Stems_log_wood = ifelse(V_carp == 0, 0,round(V_carp/V_e_,3))) %>%
      # mutate(Stems_pulp_pap = 1-Stems_log_wood) %>%
      left_join(escenario.tratamiento_0 %>% rename(Edad = edad))
    
    write.xlsx(para_excel_solo_trat, paste0(ruta_dir_escenario,"/",escenario.nombre,"_IS_",IS,"_",N_ini_,"_resumido.xlsx"))
    
    #resumen para CO2Fix
    para_CO2Fix <- para_excel_solo_trat %>%
       select(Edad,Ho_,N_a_,Dg_a_,tratamiento,Stems,CAI, Rel_growth_foliage, Rel_growth_branches, Rel_growth_roots,
             Mortalidad_natural, Mortality,
             Thinning_Harvest,Fraction_removed,Stems_log_wood, Stems_pulp_pap) 
    write.xlsx(para_CO2Fix, paste0(ruta_dir_escenario,"/",escenario.nombre,"_IS_",IS,"_",N_ini_,"_CO2Fix.xlsx"))
    
    #piezas del archivo de CO2Fix
    #-------------------------------------------------------------------------------
    load("datos/escrib_CO2fix/encabezamientos/encabezados_CO2Fix")
    id_arch_ <- c("# stems", "# foliage", "# branches", "# roots", "# mortality table", "# thinning and harvest table" )
    id_co2fix <- c("CAI", "Rel_growth_foliage","Rel_growth_branches","Rel_growth_roots", "Mortality", "Thinning_harvest") 
    id_variab_CO2Fix <- data.frame(id_arch_ = id_arch_, id_co2fix = id_co2fix)
    funcion_archivos_CO2_fix <- function(id_arch = 6, para_CO2Fix_ = "para_CO2Fix") {
      etiqueta_ = id_variab_CO2Fix$id_arch_[id_arch]
      columna_ = id_variab_CO2Fix$id_co2fix[id_arch]
      res_CO2Fix = get(para_CO2Fix_)
      
      if(etiqueta_ == "# thinning and harvest table") {
        arch_etiqueta <- res_CO2Fix %>%
          filter(Thinning_Harvest==1) %>%
          mutate(columna_texto = paste0("\t\t\t\t\t",Edad,"\t",
                                        Fraction_removed,"\t",Stems_log_wood,"\t",Stems_pulp_pap,"\t0\t0\t0.0\t0.0\t0\t0")) %>%
          select(columna_texto) %>%
          mutate(etiqueta = etiqueta_)
        
        assign(paste0("archiv_",etiqueta_), encabezados_CO2Fix)
        
        arch_CO2 <- encabezados_CO2Fix %>%
          filter(etiqueta == etiqueta_) %>%
          bind_rows(arch_etiqueta)
        
        #assign(paste0("archiv_",etiqueta_), arch_CO2, envir = .GlobalEnv)
        write.csv(arch_CO2, paste0(ruta_dir_escenario,"/","archiv_",etiqueta_,".csv"))
      }
      
      else if(etiqueta_ == "# mortality table") {
        arch_etiqueta <- res_CO2Fix %>%
          filter(Mortalidad_natural==1) %>%
          mutate(Mortality = 0) %>% #para evitar que CO2Fix interpole erróneamente la mortalidad entre turnos
          mutate(columna_texto = paste0("\t\t\t\t\t",Edad,"\t",Mortality)) %>%
          select(columna_texto) %>%
          mutate(etiqueta = etiqueta_)
        
        # arch_etiqueta <- res_CO2Fix %>%
        #   filter(Mortalidad_natural==1) %>%
        #   mutate(columna_texto = paste0("\t\t\t\t\t",Edad,"\t",Mortality)) %>%
        #   select(columna_texto) %>%
        #   mutate(etiqueta = etiqueta_)
        
        assign(paste0("archiv_",etiqueta_), encabezados_CO2Fix)
        
        arch_CO2 <- encabezados_CO2Fix %>%
          filter(etiqueta == etiqueta_) %>%
          bind_rows(arch_etiqueta)
        
        #assign(paste0("archiv_",etiqueta_), arch_CO2, envir = .GlobalEnv)
        write.csv(arch_CO2, paste0(ruta_dir_escenario,"/","archiv_",etiqueta_,".csv"))
      }
      
      else {
        arch_etiqueta <- res_CO2Fix %>%
          filter(Stems==1) %>%
          mutate(columna_texto = paste0("\t\t\t\t\t",Edad,"\t",get(columna_))) %>%
          select(columna_texto) %>%
          mutate(etiqueta = etiqueta_)
        
        assign(paste0("archiv_",etiqueta_), encabezados_CO2Fix)
        
        arch_CO2 <- encabezados_CO2Fix %>%
          filter(etiqueta == etiqueta_) %>%
          bind_rows(arch_etiqueta)
        
        #assign(paste0("archiv_",etiqueta_), arch_CO2, envir = .GlobalEnv)
        write.csv(arch_CO2, paste0(ruta_dir_escenario,"/","archiv_",etiqueta_,".csv"))
      }
      
    }
    
    map(seq(1,nrow(id_variab_CO2Fix)), ~ funcion_archivos_CO2_fix(id_arch = .[1],
                                                                  para_CO2Fix_ = "para_CO2Fix"))
    
    ############################################################################
    ############################################################################
    #lanzar los escenarios de transformacion a irregular
    gg_reg <- res %>%
      filter(Edad %in% tipos_masa$Edad) %>%
      select(Edad, N_a_, G_a_, V_a_) %>%
      mutate(N_a_ = as.integer(N_a_),
             G_a_ = round(G_a_, 2),
             V_a_ = round(V_a_, 1)) %>%
      mutate(escenario_transformacion = escenario.nombre) %>%
      left_join(tipos_masa) %>%
      rowwise() %>%
      mutate(inicio_reg = list(c(escenario_transformacion,N_a_,G_a_,Edad,V_a_,tipo_m))) %>%
      ungroup()
    
    # gg_reg <- res %>%
    #   filter(Edad %in% c(30,50,70,100)) %>%
    #   select(Edad, N_a_, G_a_) %>%
    #   mutate(N_a_ = as.integer(N_a_),
    #          G_a_ = round(G_a_, 2)) %>%
    #   mutate(escenario_transformacion = escenario.nombre) %>%
    #   rowwise() %>%
    #   mutate(inicio_reg = list(c(escenario_transformacion,N_a_,G_a_,Edad))) %>%
    #   ungroup()
    
    #Denominación del grupo
    grupo_transformacion = "transformacion" #"transformacion"
    
    #datos de inicio del monte regular, según la ordenación de Aralar
    #inicio_monte_regular_aralar <- read.csv2("datos/escenarios/aralar/inicio_monte_regular_aralar.csv")
    #inicio_monte_regular_aralar <- read.csv2("datos/escenarios/aralar/corto_inicio_monte_regular_aralar.csv") # sólo 6 escenarios
    # inicio_monte_regular_aralar <- read.csv2("datos/escenarios/prueba/prueb_transf.csv") # sólo 1 escenario
    # inicio_monte_regular_aralar_0 <- inicio_monte_regular_aralar %>%
    #   mutate(Edad_3 = round((Edad_1+Edad_2)/20)*10) %>%
    #   pivot_longer(cols = starts_with("Edad"), names_to = "clase_Edad", values_to = "Edad_reg") %>%
    #   filter(Edad_reg > 0) %>% #no tiene sentido contar con G y N >0 para Edad =0
    #   rowwise() %>%
    #   mutate(inicio_reg = list(c(escenario_Aralar,N,G, Edad_reg))) %>%
    #   ungroup()
    
    #Índice de sitio, se asimilan las calidades mala, media, buena... a las de montes regulares
    #IS_ = c(25) #c(13,16,19,22,25)
    
    #área basimétrica objetivo
    ab_objetivo_ = c(25,30) # c(15,20,25)
    precis_ab = 0.05 #margen para el ab_objetivo
    
    #rango de diámetros
    diam_max_ = c(60) #seq(60.5,100.5, by =5) #seq(60.5,100.5, by =20)
    diam_min_ = 7.5 #inicialmente se partía de 7,5 pero en la función de Weibull se llega entre 4 y 5 para las primeras edades modelizadas
    
    #peso de la intervención, tanto por uno en área basimétrica
    peso_G_ = c(0.2) #c(0.20, 0.225, 0.25)
    #años entre intervenciones, rotación
    rota_ = c(10) #c(5,7,10)
    
    #data frame de condiciones de escenario para transformacion a monte irregular
    gg_ <- expand.grid(inicio_reg = gg_reg$inicio_reg,
                      IS = IS,
                      ab_objetivo = ab_objetivo_,
                      peso_G = peso_G_,
                      diam_max = diam_max_,
                      diam_min = diam_min_,
                      rotacion = rota_)
    #lanzar función para pasar a irregular
    # map(seq(1,nrow(gg_)), ~ funcion_genera_escenarios_transformacion_desde_REG(escenario = .[1],  grupo = grupo_transformacion,
    #                                                                            gg = gg_, para_excel_solo_trat_reg = para_excel_solo_trat))
    
    
    map(seq(1,nrow(gg_)), ~ funcion_genera_escenarios_transformacion_desde_REG_unidos_(escenario = .[1],  grupo = grupo_transformacion,
                                                                                gg = gg_, para_excel_solo_trat_reg = para_excel_solo_trat, res_reg = res))
    
    
    #Resumen del escenario
    retorno <- data.frame(grupo = grupo, escenario = escenario.nombre,
                          N_ini = N_ini, IS = IS, Rotacion = edad_fin,
                          Suma_Crec_corriente = round(sum(res$Crec_corr,na.rm = TRUE),0),
                          Suma_Vol_extraido = round(sum(para_excel_solo_trat$V_e_, na.rm = TRUE),0),
                          Suma_Vol_carpinteria = round(sum(para_excel_solo_trat$V_carp),0))

}


