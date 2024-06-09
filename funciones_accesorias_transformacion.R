#Funciones accesorias para generar escenarios de transformación

#modelos
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

#relación altura ~ diámetro
load("datos/mod_altura_diametro")

# parámetros relación Dm ~ f(Dg, N, G)
# se emplea la relación de masas regulares
a7 = 0.191406636051764
a8 = -0.000620621044818245
a10 = 0.0166602839868406

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
  #se asume que el diámetro mínimo para monte irregularl es 7.5 cm
  if (a < 7.5) {
    a = 7.5
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
      mutate(n_d_despues = n_d/sum(n_d, na.rm = TRUE)*N_despues) %>%
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
      mutate(n_d_despues = n_d/sum(n_d, na.rm = TRUE)*N_despues) %>%
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
      mutate(n_d_despues = n_d/sum(n_d, na.rm = TRUE)*N_despues) %>%
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
      mutate(n_d = n_d/sum(n_d, na.rm = TRUE)*N_despues) %>%
      ungroup() %>%
      select(d_, n_d, clase_D)
    
    #actualizar los parámetros dasocráticos tras la corta
    tratamiento[i] <<- "entresaca"
    N_d[i] <<- sum(distrib_d$N_despues, na.rm = TRUE)
    G_d[i] <<- sum(distrib_d$G_despues, na.rm = TRUE)
    Dg_d[i] <<- sqrt(G_d[1]*(40000/(pi*N_d[1])))
    dist_D_d[[i]] <<- distrib_d
    dist_d_cm_d[[i]] <<- df_despues
    V_d[i] <<- sum(distrib_d$V_despues, na.rm = TRUE)
    
    # Volumen de carpintería
    V_carp[i] <<- sum(df_vcarp_biom$v_carp, na.rm = TRUE)
    
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
    mutate(ab_ifn3 = sum(ab_, na.rm = TRUE))
  
  actualiza_1 <- actualiza_0 %>%
    mutate(inc_ab_pred = predict(mod.Biandi_nlm, newdata = newdata_)) %>% 
    mutate(inc_ab_corregido = int_corr_ab + slo_corr_ab*inc_ab_pred) %>% #corrección
    #mutate(inc_ab_corregido = int_corr_ab + inc_ab_pred) %>% 
    mutate(D_act = sqrt((inc_ab_corregido+pi*(d_/2)^2)*4/pi))
  
  df_ <- actualiza_1 %>%
    select(D_act, n_d) %>%
    rename(d_ = D_act) %>%
    mutate(clase_D = cut(d_, breaks = breaks_5cm, labels = seq(5,150,by=5))) %>% #labels identifica el diámetro medio de la clase
    mutate(clase_D = as.numeric(as.character(clase_D)))
  
  #se asegura que se incorporan a la clase inferior para alcanzar el número de árboles de la curva teórica en esa clase
  #se añaden los árboles necesarios al diámetro inferior, 7,5 cm
  sum_cla_inferior = sum(df_$n_d[df_$clase_D == min(N_x5$clase_D, na.rm = TRUE)], na.rm = TRUE) #árboles en la clase inferior
  teor_cla_inferior =   N_x5$N_x_y[which.min(N_x5$clase_D)] #árboles en la clase inferior según la curva objetivo
  
  if (sum_cla_inferior < teor_cla_inferior) {
    df_clas_min <- data.frame(d_ = min(N_x5$diam_min_, na.rm = TRUE),
                              n_d = teor_cla_inferior - sum_cla_inferior,
                              clase_D = min(N_x5$clase_D, na.rm = TRUE)) 
    df_ <- df_ %>%
      filter(clase_D >  min(N_x5$diam_min_, na.rm = TRUE)) %>%
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
    mutate(VCC = sum(vol, na.rm = TRUE),
           bal_mod_gih = bal/(sum(ab, na.rm = TRUE)*IH),
           rddg = d_/Dg,
           N = sum(n_d, na.rm = TRUE),
           G = sum(ab, na.rm = TRUE)) %>%
    mutate( v_carp_1 = exp(-4.1871492+0.0029901*VCC-0.0178539*bal_mod_gih^2),
            v_carp_2 = exp(-10.1197792+2.0351346*rddg+0.1360856*Dg+0.0012059*N),
            v_carp_3 = exp(-5.2618050+0.0679419*d_+0.0004858*N),
            v_carp_4 = exp(-6.783112002+0.082219560*d_+0.003219738*N-0.000002936*N^2+0.026264302*G)) %>%
    mutate(v_carp = v_carp_1+v_carp_2+v_carp_3+v_carp_4) %>%
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
    summarise(N_antes = sum(n_d, na.rm = TRUE),
              G_antes = sum(ab, na.rm = TRUE),
              V_antes = sum(vol, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(extra_ = (clase_D >= max(N_x5$clase_D, na.rm = TRUE))) %>%
    mutate(suma_extra_N = sum(N_antes*extra_, na.rm = TRUE),
           suma_extra_G = sum(G_antes*extra_, na.rm = TRUE),
           suma_extra_V = sum(V_antes*extra_, na.rm = TRUE)) %>%
    mutate(N_antes = ifelse(clase_D == max(N_x5$clase_D, na.rm = TRUE), suma_extra_N, N_antes),
           G_antes = ifelse(clase_D == max(N_x5$clase_D, na.rm = TRUE), suma_extra_G, G_antes),
           V_antes = ifelse(clase_D == max(N_x5$clase_D, na.rm = TRUE), suma_extra_V, V_antes)) %>%
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
  N_a[i] <<- sum(df_clase$N_antes, na.rm = TRUE)
  G_a[i] <<- sum(df_clase$G_antes, na.rm = TRUE)
  Dg_a[i] <<- sqrt(G_a[i]*(40000/(pi*N_a[i])))
  dist_D_a[[i]] <<- df_clase
  #dist_d_cm_a[[i]] <<- df_
  dist_d_cm_a[[i]] <<- df_v_carp
  V_a[i] <<- sum(df_clase$V_antes, na.rm = TRUE)
  
}

