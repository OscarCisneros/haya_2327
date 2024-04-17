library(tidyverse)
qq <- readLines("datos/escrib_CO2fix/escenario_Q1.txt")
ejemp_sel_ref <- readLines("datos/escrib_CO2fix/ejemp_selv_ref.txt")
ejemp_CO2Fix <-readLines("datos/escrib_CO2fix/Q1_ARTIKUTZA.co2") 
foliage <- readLines("datos/escrib_CO2fix/foliage.txt")
foliage_2 <- readLines("datos/escrib_CO2fix/foliage_2.txt")
qq_export <- gsub(pattern = foliage, replace = foliage_2, x = qq)

str_replace(string = uuu, pattern = uu, replacement = uu_2)

uu <- "\t\t\t\t# foliage"
ii <- "\t\t\t\t# florero"
oo <- str_replace(string = foliage, pattern = uu, replacement = ii)
exporta_foliage <- writeLines(oo, con = "datos/escrib_CO2fix/exporta_foliage.txt")

#-------------------------------------------------------------------------------
etiquetas <- c("\t\t\t\t# stems", "\t\t\t\t# foliage", "\t\t\t\t# branches", "\t\t\t\t# roots",
               "\t\t\t\t# mortality table", "\t\t\t\t# thinning and harvest table" )


#rr <- data.frame(columna_texto = qq) %>%
  rr <- data.frame(columna_texto = ejemp_sel_ref) %>%
  mutate(TF_etiqueta = columna_texto %in% etiquetas) %>%
  mutate(marcar_llave = columna_texto == "\t\t\t\t}") %>% #marcar las líneas con llave de cierre después de cada etiqueta
  mutate(etiqueta = ifelse(TF_etiqueta | marcar_llave, columna_texto, NA)) %>%
  fill(etiqueta) %>%
  mutate(etiqueta = ifelse(is.na(etiqueta), 0, etiqueta)) %>%
  mutate(orden_trozos = cumsum(etiqueta != lag(etiqueta, default = first(etiqueta)))) #indica el orden de cada trozo del dataframe

  #dataframe con un ejemplo de escenario, para usarlo de plantilla
  plantilla_escenario_CO2Fix <- data.frame(columna_texto = ejemp_sel_ref) %>%
    mutate(TF_etiqueta = columna_texto %in% etiquetas) %>%
    mutate(marcar_llave = columna_texto == "\t\t\t\t}") %>% #marcar las líneas con llave de cierre después de cada etiqueta
    mutate(etiqueta = ifelse(TF_etiqueta | marcar_llave, columna_texto, NA)) %>%
    fill(etiqueta) %>%
    mutate(etiqueta = ifelse(is.na(etiqueta), 0, etiqueta)) %>%
    mutate(orden_trozos = cumsum(etiqueta != lag(etiqueta, default = first(etiqueta)))) #indica el orden de cada trozo del dataframe
  write.csv(plantilla_escenario_CO2Fix, "datos/escrib_CO2Fix/encabezamientos/plantilla_escenario_CO2Fix.csv")
  save(plantilla_escenario_CO2Fix, file = "datos/escrib_CO2Fix/encabezamientos/plantilla_escenario_CO2Fix" )
  
  #dataframe con un archivo CO2Fix completo
  #rr <- data.frame(columna_texto = qq) %>%
  df_ejemp_CO2Fix <- data.frame(columna_texto = ejemp_CO2Fix) %>%
    mutate(TF_etiqueta = columna_texto %in% etiquetas) %>%
    mutate(marcar_llave = columna_texto == "\t\t\t\t}") %>% #marcar las líneas con llave de cierre después de cada etiqueta
    mutate(etiqueta = ifelse(TF_etiqueta | marcar_llave, columna_texto, NA)) %>%
    fill(etiqueta) %>%
    mutate(etiqueta = ifelse(is.na(etiqueta), 0, etiqueta)) %>%
    mutate(orden_trozos = cumsum(etiqueta != lag(etiqueta, default = first(etiqueta)))) %>% #indica el orden de cada trozo del dataframe
    select(columna_texto)
  
  encabezamiento_general_CO2Fix <- df_ejemp_CO2Fix[1:63,]
  write.csv(encabezamiento_general_CO2Fix, "datos/escrib_CO2Fix/encabezamientos/encabezamiento_general_CO2Fix.csv")
  save(encabezamiento_general_CO2Fix, file = "datos/escrib_CO2Fix/encabezamientos/encabezamiento_general_CO2Fix" )
#-------------------------------------------------------------------------------
id_arch_ <- c("# stems", "# foliage", "# branches", "# roots", "# mortality table", "# thinning and harvest table" )
id_co2fix <- c("CAI", "Rel_growth_foliage","Rel_growth_branches","Rel_growth_roots", "Mortality", "Thinning_harvest") 
id_variab_CO2Fix <- data.frame(id_arch_ = id_arch_, id_co2fix = id_co2fix)
funcion_archivos_CO2_fix <- function(id_arch = 6, para_CO2Fix_ = "cod_1_1_8_17_IS25_alta_IS_25_CO2Fix") {
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
      mutate(columna_texto = paste0("\t\t\t\t\t",Edad,"\t",Mortality)) %>%
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
                                                              para_CO2Fix_ = "cod_1_1_8_17_IS25_alta_IS_25_CO2Fix"))

#-------------------------------------------------------------------------------
nombre_etiqueta <- rr %>% select(columna_texto, etiqueta, orden_trozos)
split_df <- split(nombre_etiqueta, nombre_etiqueta$orden_trozos)

split_df[["1"]][["etiqueta"]][[1]] %in% paste0("\t\t\t\t",id_arch_[1])  
  arch_n <- get(paste0("archiv_",id_arch_[1])) %>%
    mutate(orden_trozos = split_df[["1"]][["orden_trozos"]][[1]])
  split_df[["1"]] <- arch_n
  oo <- data.frame()
  for (i in unique(nombre_etiqueta$orden_trozos)) {
    i = as.character(i)
    oo <- oo %>% bind_rows(split_df[[i]])  }
  
  
  #-------------------------------------------------------------------------------  
  n_esc_ori <- "7\t# number of Scenarios"
  n_esc_nuevo <- "1\t# number of Scenarios"
  n_mit_ori <- "6\t# mitigation scenario"
  n_mit_nuevo <- "0\t# mitigation scenario"
  encabezamiento_general_prueba <- str_replace(string = encabezamiento_general, pattern = n_esc_ori, replacement = n_esc_nuevo)
  encabezamiento_general_prueba <- str_replace(string = encabezamiento_general_prueba, pattern = n_mit_ori, replacement = n_mit_nuevo)
  
  ii <- data.frame(columna_texto = encabezamiento_general_prueba) %>%
    bind_rows(rr %>% select(columna_texto))
  writeLines(ii$columna_texto, con = "datos/escrib_CO2fix/ii.co2")
  
  
  ff <- data.frame(columna_texto = encabezamiento_general_prueba) %>%
    bind_rows(oo %>% select(columna_texto))
  writeLines(ff$columna_texto, con = "datos/escrib_CO2fix/ff.co2")