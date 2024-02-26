#Simulación de clara


library(tidyverse)
#library(NLRoot)

# Escenario
      # Denominación del escenario
      escenario.nombre <- "H1"
      
      #esquema de tratamientos
      escenario.tratamiento <- read.csv("datos/escenarios/go_fagus/tratamientos_H1.csv", sep=";")
#modelo de incremento individual
      load("datos/mod_inc_diam_individual")

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
      
      
      
      # densidad inicial
      N_ini = 7000
      # densidad final
      N_fin = 250
      # clareo
      clareo = 3500
      # extracción mínima en clara, m3/ha
      extrac_min = 40
      
      
      # tasa de mortalidad natural en 5 años
      mort_lustro = 0.02141
      # parámetros del modelo de mortalidad SFA
      mort_interc <- 13.046138
      mort_pend <- -1.90102
      
      # calidad de estación
      IS = 25
      
      # rango de edades
      edad_ini <- 5
      edad_fin <- 120



edad = seq(edad_ini,edad_fin, by = 5)
Ho = IS*((1-1/exp(0.02*edad))/(1-1/exp(0.02*80)))^1.4823

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
        N_extr_nat = N_*(1-mort_lustro)
        N_extr_SFA = exp(mort_interc+log(D_)*mort_pend)
        return(min(c(N_extr_nat,N_extr_SFA)))
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
         
         Dm_ = Dg_ - exp(a7+a8*N_+a10*G_)
         var_ = Dg_^2 - Dm_^2
         # parámetro a
         if (is.null(dist_D_d[[i_-1]])) {
           a = 5
         } else {
           # min(dist_D_d[i_-1]$d)
           newdata_ = data.frame(Ho_ifn3 = Ho[i_-1],
                                 ab_ifn3 = (Dg_d[i_-1])^2*pi*(N_[i_-1])/40000,
                                 Dn_ifn3 = min(dist_D_d[[i_-1]]$d),
                                 dgm_ifn3 = Dg_d[i_-1])
           inc_ <- predict(m.Richards, newdata = newdata_)
           a = min(dist_D_d[[i_-1]]$d)+inc_
         }

         
         # parámetro c
         func = function(x){
           var_ -  ((Dm_ - a)^2 / (gamma(1+1/x))^2) *(gamma(1+2/x) - (gamma(1+1/x))^2)
         }
         c = Bisection(f = func,1,4)
         # parámetro b
         b = (Dm_ - a)/gamma(1+1/c)
         return(c(a,b,c))
      }
      
      #función de incremento diametral
      #modelo de incremento anual, sin outliers
      # load("datos/mod_inc_diam_individual")
      # func_inc_diam <- functio(i_= 7) {
      #   
      #   param_= func_expande_Weibull(i_)
      #   df <- data.frame(d_ = seq(param_[1]+0.5,100)) %>% #100 como diámetro superior, pero deberíamos parametrizarlo
      #     mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))* N_) %>%
      #     mutate(n_d = floor(n_d)) %>%
      #     mutate(diam2xN = d_^2*n_d) %>% #estimación de la relación diámetro-volumen
      #     mutate(vol_prop = V_total*diam2xN/sum(diam2xN)) %>% #volumen repartido proporcionalmente por diámetro
      #     #arrange(d_) %>% #operaciones para marcar los diámetros por lo bajo hasta V_extr
      #     mutate(cum_ = cumsum(vol_prop)) %>%
      #     mutate(cum_1 = cum_-V_extr) %>%
      #     mutate(signo_cum_= sign(cum_1)) %>%
      #     mutate(diff_ = c(0, diff(signo_cum_))) %>%
      #     #mutate(lag_diff = lead(diff_)) %>%
      #     #mutate(mult = (signo_cum_ == -1)*vol_prop + (diff_ == 2)*lag(cum_1) ) %>%
      #     mutate(mult = (signo_cum_ == -1)*vol_prop + (diff_ == 2)* c(0, cum_1[1:(n()-1)]) ) %>%
      #     mutate(vol_clase = abs(mult)) %>%
      #     mutate(n_extraido_clase = vol_clase/vol_prop*n_d) %>%
      #     mutate(n_despues_clase = n_d - n_extraido_clase) %>%
      #     filter(n_despues_clase > 0) 
      #   new_data_ = 
      #   inc_an  ~ (a1*Ho_ifn3^a2 + a3*ab_ifn3 + a4*(Dn_ifn3/dgm_ifn3)) * 
      #     ((1-exp(-b2*Dn_ifn3))^(b3-1))*exp(-b2*Dn_ifn3)
      # }
      # uu <- predict(m.Richards, newdata = dat_inc)
      # evalua <- dat_inc %>%
      #   mutate(predicho = uu) %>%
      #   mutate(predicho_Dn = Dn_ifn3+predicho*anno) %>%
      #   mutate(residuo_Dn = Dn - predicho_Dn)
      # 

     
      # función de clara por lo bajo
      func_clara_bajo <- function(i_ = 1) {
        N_a_ = N_d[i_-1]
        Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
        V_a_ = b3*Dg_a_^b4*Ho[i_]^b5*N_a_^b6
        dist_D_a_ <- NA #no hace falta calcularlo
        
        V_total = V_a_
        V_extr = extrac_min
        #V_total =  b3*Dg_^b4*Ho[i]^b5*N_^b6
        
        param_= func_expande_Weibull(i_, Dg_a_, N_a_) #tiene en cuanta la variación de "a", diámetro mínimo
        df <- data.frame(d_ = seq(param_[1]+0.5,100)) %>% #100 como diámetro superior, pero deberíamos parametrizarlo
          mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3]))* N_a_) %>%
          mutate(n_d = floor(n_d)) %>%
          mutate(diam2xN = d_^2*n_d) %>% #estimación de la relación diámetro-volumen
          mutate(vol_prop = V_total*diam2xN/sum(diam2xN)) %>% #volumen repartido proporcionalmente por diámetro
          #arrange(d_) %>% #operaciones para marcar los diámetros por lo bajo hasta V_extr
          mutate(cum_ = cumsum(vol_prop)) %>%
          mutate(cum_1 = cum_-V_extr) %>%
          mutate(signo_cum_= sign(cum_1)) %>%
          mutate(diff_ = c(0, diff(signo_cum_))) %>%
          #mutate(lag_diff = lead(diff_)) %>%
          #mutate(mult = (signo_cum_ == -1)*vol_prop + (diff_ == 2)*lag(cum_1) ) %>%
          mutate(mult = (signo_cum_ == -1)*vol_prop + (diff_ == 2)* c(0, cum_1[1:(n()-1)]) ) %>%
          mutate(vol_clase = abs(mult)) %>%
          mutate(n_extraido_clase = vol_clase/vol_prop*n_d) %>%
          mutate(n_despues_clase = n_d - n_extraido_clase) %>%
          filter(n_despues_clase > 0) #eliminar las clases sin árboles
         
        N_d_ = sum(df$n_despues_clase)
        Dg_d_ = sqrt(sum(df$n_despues_clase*(df$d_-0.5)^2)/sum(df$n_despues_clase))
        #V_d_ = V_total-V_extr
        V_d_ = sum(df$vol_prop + df$mult)
        dist_D_d_ <- df
        

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
         } else if (is.null(dist_D_d[[i_-1]])) {
           param_= func_expande_Weibull(i_, Dg_a_, N_a_)
           df <- data.frame(d_ = seq(param_[1]+0.5,100)) %>% #100 como diámetro superior, pero deberíamos parametrizarlo
             mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3])) * N_a_) %>%
             mutate(n_d = floor(n_d)) %>%
             filter(n_d > 0)
           dist_D_a_ <- df
         }
                          
         #después
         N_d_ = func_mort(N_d[i_-1], Dg_d[i_-1])
         Dg_d_ = b0*N_d_^b1*Ho[i_]^b2
         V_d_ <- b3*Dg_d_^b4*Ho[i_]^b5*N_d_^b6
         dist_D_d_ <- dist_D_a_ #esta distribución sólo se usa a efectos del cálculo del diámetro mínimo.
                                #se asume que el diámetro mínimo no cambia tras la mortalidad natural, es un fenómeno aleatorio
         # N_d_ = func_mort(N_d[i_-1], Dg_d[i_-1])
         # Dg_d_ = b0*N_d_^b1*Ho[i]^b2
         # V_d_ <- b3*Dg_d_^b4*Ho[i]^b5*N_d_^b6
 
         return(list(N_a_,Dg_a_,V_a_,dist_D_a_, N_d_,Dg_d_,V_d_,dist_D_d_))
         
         } else if (tratam_ == "clareo") {
           #antes
           N_a_ <- N_d[i_-1]
           Dg_a_ = b0*N_a_^b1*Ho[i_]^b2
           V_a_ <- b3*Dg_a_^b4*Ho[i_]^b5*N_a_^b6
           if (is.na(mod_evol_D$id[i_])) {
             dist_D_a_ <- NA
           } else if (is.null(dist_D_d[[i_-1]])) {
             param_= func_expande_Weibull(i_, Dg_a_, N_a_)
             df <- data.frame(d_ = seq(param_[1]+0.5,100)) %>% #100 como diámetro superior, pero deberíamos parametrizarlo
               mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_[1],param_[2],param_[3])) * N_a_) %>%
               mutate(n_d = floor(n_d)) %>%
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
         } else if (tratam_ %in% c("clara por lo bajo","clara mixta","diseminatoria","aclaratoria 1")) {
           func_clara_bajo(i_= i_)
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
      for (i in c(2:23)) { 
        #N_a[i] <- N_d[i-1]
        print(i)
        avance_ <- func_trat(tratam = escenario.tratamiento$tratamiento[i], i_ = i)
        #N_d[i] <- func_trat(tratam = escenario.tratamiento$tratamiento[i], i_ = i)
        N_a[i] <- avance_[[1]]
        Dg_a[i] <- avance_[[2]]
        V_a[i] <- avance_[[3]]
        dist_D_a[[i]] <- avance_[[4]]
        N_d[i] <- avance_[[5]]
        Dg_d[i] <- avance_[[6]]
        V_d[i] <- avance_[[7]]
        dist_D_a[[i]] <- avance_[[8]]
      }

  res <- data.frame(N_a_ = N_a, Dg_a_ = Dg_a, V_a_ = V_a,
                    N_d_ = N_d, Dg_d_ = Dg_d, V_d_ = V_d,
                    tratam = escenario.tratamiento$tratamiento)
  write.csv2(res, "res_H1_primera_sin_secos.csv")
  