#función para simular resultados del proceso de transformación
#a partir del código simula_transformacion_x_cm.R

library(openxlsx)
library(zoo)
library(ggnewscale)
library(tidyverse)

#Variables para la simulación 
  # grupo, escenario.nombre
  # t_fin, duración de la simulación
  # IS (c(13,16,19,22,25))
  # Datos de inicio de monte regular: Edad, N, G. A partir de las tablas de la ordenación de Aralar. Importado inicio_monte_regular_aralar.csv
  # ab_objetivo: área basimétrica objetivo (según el documento de Aralar 15, 20, 25)
  # precis_ab: margen para el cálculo de ab_objetivo (p.e. 0.05), inicialmente se deja fijo
  # Rango de diámetro d_min: 7,5. d_max = 60.5 a 100.5. El diámetro máximo es variable en la transformación, el mínimo se puede dejar fijo
  # peso_G = peso de la intervención en tanto por un de área basimétrica. EN la ordenación entre 0.20 y 0.25 (pe 0.20, 0.225, 0.25)
  # rotación, tiempo entre intervenciones. Máximo de 10 años (pe 5, 7, 10)

#cargar las funciones accesorias
source("scripts/funciones_accesorias_transformacion.R")

#Denominación de las claras tratadas con la función de claras mixtas
tipos_claras <- c("clara por lo bajo","clara mixta","diseminatoria","aclaratoria 1", "aclaratoria","clara selectiva","corta preparatoria","corta diseminatoria", "entresaca")

#datos de claras para el gráfico
  res_daso_claras <- read.csv2("datos/claras/res_daso_claras.csv")
  res_daso_claras_0 <- res_daso_claras %>%
    pivot_wider(names_from = "var", values_from = "valor")

#datos de ordenaciones para el gráfico
  dat_ordenaciones_adultas <- read.csv("resultados/dat_ordenaciones_adultas.csv")

#Duración de la simulación
  t_fin = 300
  
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
 
#Denominación del grupo
grupo = "prueba" #"transformacion"

#datos de inicio del monte regular, según la ordenación de Aralar
#inicio_monte_regular_aralar <- read.csv2("datos/escenarios/aralar/inicio_monte_regular_aralar.csv")
#inicio_monte_regular_aralar <- read.csv2("datos/escenarios/aralar/corto_inicio_monte_regular_aralar.csv") # sólo 6 escenarios
inicio_monte_regular_aralar <- read.csv2("datos/escenarios/prueba/prueb_transf.csv") # sólo 1 escenario
inicio_monte_regular_aralar_0 <- inicio_monte_regular_aralar %>%
  mutate(Edad_3 = round((Edad_1+Edad_2)/20)*10) %>%
  pivot_longer(cols = starts_with("Edad"), names_to = "clase_Edad", values_to = "Edad_reg") %>%
  filter(Edad_reg > 0) %>% #no tiene sentido contar con G y N >0 para Edad =0
  rowwise() %>%
  mutate(inicio_reg = list(c(escenario_Aralar,N,G, Edad_reg))) %>%
  ungroup()

#Índice de sitio, se asimilan las calidades mala, media, buena... a las de montes regulares
IS_ = c(25) #c(13,16,19,22,25)

#área basimétrica objetivo
ab_objetivo_ = c(30) # c(15,20,25)
precis_ab = 0.05 #margen para el ab_objetivo

#rango de diámetros
diam_max_ = seq(65.5,100.5, by =10) #seq(60.5,100.5, by =5)
diam_min = 7.5

#peso de la intervención, tanto por uno en área basimétrica
peso_G_ = c(0.20, 0.25) #c(0.20, 0.225, 0.25)
#años entre intervenciones, rotación
rota_ = c(10) #c(5,7,10)


gg <- expand.grid(inicio_reg = inicio_monte_regular_aralar_0$inicio_reg,
                  IS = IS_,
                  ab_objetivo = ab_objetivo_,
                  peso_G = peso_G_,
                  diam_max = diam_max_,
                  rotacion = rota_)

funcion_genera_escenarios_transformacion <- function( escenario = 1) {
 
print(paste0("escenario nº: ",escenario))
  
  #Índice de sitio, se asimilan las calidades mala, media, buena... a las de montes regulares
  IS = gg[escenario, "IS"]
  
  #Datos del monte regular
  #escenario.nombre <-paste0(gg[escenario, "inicio_reg"][[1]][1],"_", escenario)
  N_reg <- as.numeric(gg[escenario, "inicio_reg"][[1]][2])
  G_reg <- as.numeric(gg[escenario, "inicio_reg"][[1]][3])
  Edad_reg <- as.numeric(gg[escenario, "inicio_reg"][[1]][4])
  
  #área basimétrica objetivo
  ab_objetivo <<- gg[escenario, "ab_objetivo"]
  precis_ab = 0.05 #margen para el ab_objetivo
  #peso de la intervención, tanto por uno en área basimétrica
  peso_G <<- gg[escenario, "peso_G"]
  #años entre intervenciones, rotación
  rota <<-  gg[escenario, "rotacion"]
  #rango de diámetros
  diam_max <<- gg[escenario, "diam_max"]
  diam_min = 7.5
  
  
  
  #carpeta de escenarios
  escenario.nombre = paste0("E_",Edad_reg,"_N_",N_reg,"_IS_",IS, "_DMX_",as.character(diam_max*10))
  ruta_dir_escenario = paste0("resultados/simulaciones/",grupo,"/",escenario.nombre)
  if(!file.exists(ruta_dir_escenario)) {
    dir.create(ruta_dir_escenario)
  }
  
  #resumen de las variables empleadas en el escenario
  resumen_variables <- data.frame(escenario.nombre = escenario.nombre,
                                  N_reg = N_reg, G_reg = G_reg, Edad_reg = Edad_reg,
                                  ab_objetivo = ab_objetivo, precis_ab = precis_ab,
                                  peso_G = peso_G, rotacion = rota, 
                                  diam_max = diam_max, diam_min = diam_min)
  write.xlsx(resumen_variables, paste0(ruta_dir_escenario,"/resumen_variables.xlsx"))
  
  
  #corrección del crecimiento diametral
  correc_calidad_irreg$calidad <- c(13,16,19,22,25)
  int_corr_ab <<- correc_calidad_irreg$intercept[which(correc_calidad_irreg$calidad == IS)]
  slo_corr_ab <<- correc_calidad_irreg$pendiente[which(correc_calidad_irreg$calidad == IS)]
  
  
  #Determinar la curva objetivo----
      q = log(a_liocourt)/5
      K = (-40000/pi)*ab_objetivo/(exp(-q*diam_max)*(diam_max^2/q+2*diam_max/q^2+2/q^3)-
                                     exp(-q*diam_min)*(diam_min^2/q+2*diam_min/q^2+2/q^3))
      N_x5 <<- data.frame(diam_min_ = seq(diam_min, diam_max, by=5)) %>%
        mutate(diam_max_ = diam_min_+5) %>%
        mutate(N_x_y = -K/q*(exp(-q*diam_max_)- exp(-q*diam_min_))) %>%
        mutate(ab_ = pi*((diam_min_+2.5)/200)^2*N_x_y) %>%
        mutate(clase_D = diam_min_+2.5) 
  
  #Determinar la curva de inicio----
      breaks_5cm <<- seq(2.5,152.5, by = 5) #divisiones teóricas de la distribución inicial
      param_weibull_irreg <- func_expande_Weibull_irreg(N_a_ = N_reg, G_a_ = G_reg)
      #param_= func_expande_Weibull(i_, Dg_a_, N_a_) #tiene en cuanta la variación de "a", diámetro mínimo
      
      df_ <- data.frame(d_ = seq(round(param_weibull_irreg[1]/0.5)*0.5,150)) %>% #150 como diámetro superior, pero deberíamos parametrizarlo
        mutate(n_d = sapply(d_, function(x) func_dens_weibull(x,param_weibull_irreg[1],param_weibull_irreg[2],param_weibull_irreg[3]))* N_reg) %>%
        filter(!is.na(n_d)) %>%
        mutate(n_d = n_d/sum(n_d, na.rm = TRUE)*N_reg) %>% #para eliminar errores al aproximar la curva de densidad de Weibull
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
        mutate(Ho = sum(mult*Ht, na.rm = TRUE)/sum(mult, na.rm = TRUE)) %>%
        mutate(IH = 100/Ho*sqrt(20000/(sqrt(3)*sum(n_d, na.rm = TRUE)))) %>% #Índice de Hart-Becking
        mutate(Dg = sqrt(sum(n_d*d_^2)/sum(n_d, na.rm = TRUE))) %>% #Diámetro medio cuadrático
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
        mutate(v_carp = ifelse(v_carp > vol, vol, v_carp)) %>%
        mutate(v_carp = ifelse(d_ >= 20, v_carp, 0)) %>% #sólo se considera para sierra por encima de 20 cm
        #fracciones de biomasa
        mutate(B_hoja = 0.0167*d_^2.951*Ht^-1.101, #Batelink 1997
               B_fuste = exp(0.23272^2/2)*exp(-1.63732)*d_^2.21464,
               B_rama7 = exp(0.62932^2/2)*exp(-10.811)*d_^4.08961,
               B_rama2_7 = exp(0.333796^2/2)*exp(-3.86719)*d_^2.34551,
               B_rama2 = exp(0.425041^2/2)*exp(-2.57396)*d_^1.84345,
               B_raiz = 0.106*d_^2) %>% #de la tesis de Ricardo
        mutate(B_rama = B_rama7+B_rama2_7+B_rama2) %>%
        select(d_, n_d, clase_D, ab, vol, v_carp, B_fuste, B_rama, B_hoja, B_raiz)
      
      df_clase <- df_ %>%
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
        select(-extra_, -suma_extra_N,-suma_extra_G,-suma_extra_V) 
      
      #variables dasocráticas iniciales
      N_a[1] <<- sum(df_clase$N_antes, na.rm = TRUE)
      G_a[1] <<- sum(df_clase$G_antes, na.rm = TRUE)
      Dg_a[1] <<- sqrt(G_a[1]*(40000/(pi*N_a[1])))
      dist_D_a[[1]] <<- df_clase
      dist_d_cm_a[[1]] <<- df_v_carp
      V_a[1] <<- sum(df_clase$V_antes, na.rm = TRUE)
     
      
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
      
      
      
      res <<- data.frame(IS_ = IS, Tiempo = tiempo,
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
      
      
      #ggplot(res, aes(x=Tiempo, y = Crec_corr_))+geom_point()
      
      #Suavizar el dato de Crec_corr_
      # library(splines)
      # 
      # m.Crec_corr <-lm(res$Crec_corr_~bs(res$Edad ))
      #res$Crec_corr_ <- c(0, predict(m.Crec_corr))
      
      res$Crec_corr_ <- rollmedian(res$Crec_corr_, 5, fill = "extend")
      res$Crec_corr <-  res$Crec_corr_
      
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
      write.xlsx(para_excel_res, paste0(ruta_dir_escenario,"/",escenario.nombre,".xlsx"))
      
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
        ggtitle(paste0("Datos de ordenaciones. ",grupo,"/",escenario.nombre),
                subtitle = paste0("Tipo: Adultas susceptibles de claras"))+
        theme_light()+
        labs(x = "Edad", y = "Vcc m3/ha")+
        labs(color = "Evolución según IS")+
        labs(shape = "Clara. Tratamiento")+
        theme(text = element_text(size = 30))
      
      
      ggsave(paste0(ruta_dir_escenario,"/",escenario.nombre,".png"), width = 677.4 , height = 364.416, units = "mm")
      
      
      para_excel_solo_trat <- para_excel_res %>%
        filter(Tiempo %in% seq(5,500, by=5) | tratamiento %in% c("final","clareo",tipos_claras)) %>%
        mutate(Mortalidad_natural = ifelse(tratamiento %in% c("mortalidad natural"),1,0)) %>%
        mutate(Mortality = Mortalidad_natural*round(V_e_/V_a_,2)) %>%
        mutate(Stems = ifelse(Tiempo %in% seq(5,500, by=5), 1, 0)) %>%
        mutate(CAI = Stems*Crec_corr_) %>%
        mutate(Thinning_Harvest = ifelse(tratamiento %in% c("final","clareo",tipos_claras),1,0)) %>%
        mutate(Fraction_removed = Thinning_Harvest*round(V_e_/V_a_,2)) %>%
        mutate(Stems_log_wood = ifelse(V_carp_ == 0, 0,round(V_carp_/V_e_,3))) %>%
        mutate(Stems_pulp_pap = 1-Stems_log_wood) 
      
      write.xlsx(para_excel_solo_trat, paste0(ruta_dir_escenario,"/",escenario.nombre,"_resumido.xlsx"))
      
      #resumen para CO2Fix
      para_CO2Fix <- para_excel_solo_trat %>%
        rename(Rel_growth_foliage = Rel_growth_foliage_, Rel_growth_branches = Rel_growth_branches_, Rel_growth_roots = Rel_growth_roots_) %>%
        select(Tiempo,N_a_,Dg_a_,tratamiento,Stems,CAI, Rel_growth_foliage, Rel_growth_branches, Rel_growth_roots,
               Mortalidad_natural, Mortality,
               Thinning_Harvest,Fraction_removed,Stems_log_wood, Stems_pulp_pap) 
      write.xlsx(para_CO2Fix, paste0(ruta_dir_escenario,"/",escenario.nombre,"_CO2Fix.xlsx"))
      
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
            mutate(columna_texto = paste0("\t\t\t\t\t",Tiempo,"\t",
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
            mutate(columna_texto = paste0("\t\t\t\t\t",Tiempo,"\t",Mortality)) %>%
            select(columna_texto) %>%
            mutate(etiqueta = etiqueta_)
          
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
            mutate(columna_texto = paste0("\t\t\t\t\t",Tiempo,"\t",get(columna_))) %>%
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
      
      #Resumen del escenario
      retorno <- data.frame(grupo = grupo, escenario = escenario.nombre,
                            Tiempo = t_fin, IS = IS, Rotacion = rota,
                            Suma_Crec_corriente = round(sum(res$Crec_corr,na.rm = TRUE),0),
                            Suma_Vol_extraido = round(sum(para_excel_solo_trat$V_e_, na.rm = TRUE),0),
                            Suma_Vol_carpinteria = round(sum(para_excel_solo_trat$V_carp_),0))
      
      
}

#oo <- map_dfr(seq(23,nrow(gg)), ~funcion_genera_escenarios_transformacion(escenario = .[1]))
