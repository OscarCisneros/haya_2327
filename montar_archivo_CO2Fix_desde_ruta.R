#montar archivo CO2Fix, desde un nombre que comparten enparte varios escenarios a comparar
#a partir del encabezamiento general de un archivo CO2Fix elaboardo por Mikel
#se incluyen los datos generales de clima y destino de productos propuesto por Mikel
#se cambia el número de años de simulación, el número de escenarios y se pone a cero el escenario de simulación
#después se unen los diferentes escenarios, cada uno reorganizado para coincidir con la estructura original
library(tidyverse)


#ruta de los archivos de monte regular
grupo_ <-  "prueba" #"go_fagus" #"planes_comarcales_navarra" "selv_macizo_pirenaico" "CNPF"
densidades_iniciales <- c(7000,5000)

csv_nombres <- list.files(paste0("datos/escenarios/",grupo_), pattern = "\\.csv")
csv_nombres <- substr(csv_nombres, 1, nchar(csv_nombres)-4)

lista_ <- expand.grid(grupo_ = grupo_, densidades_iniciales = densidades_iniciales, csv_nombres = csv_nombres, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(lista_var = list(c(grupo_,densidades_iniciales, csv_nombres)))

grupo_transformacion <- "transformacion"

funcion_montar_CO2Fix_reg_a_irreg <- function(csv_nombre = "plan_comarcal_1_1_8_17_dif_dens_IS13_alta", densidades_inicial = "7000") {
  #nombre del archivo CO2Fix a generar
  archiv_para_CO2Fix = paste0(csv_nombre,"_",densidades_inicial,".co2")
  
  #cargar el encabezamiento general
  load("datos/escrib_CO2Fix/encabezamientos/encabezamiento_general_CO2Fix")
  #cargar la plantilla de escenario
  load("datos/escrib_CO2Fix/encabezamientos/plantilla_escenario_CO2Fix")
  
  #ruta de escenarios a integrar
    #ruta de escesario regular
    rutas_escenario_reg <- paste0("resultados/simulaciones/",grupo_,"/",csv_nombre, "_",densidades_inicial)
    #rutas_escenarios_irreg
    nombres_irreg <- list.files(paste0("resultados/simulaciones/",grupo_transformacion), pattern = paste0(csv_nombre,"*"))
    esc_nombres_irreg <- paste0("resultados/simulaciones/",grupo_transformacion,"/",nombres_irreg)
  rutas_escenarios <- c(rutas_escenario_reg, esc_nombres_irreg)
  
  #función para componer cada escenario
  funcion_compone_escenario <- function(id_ = 1) {
    ruta_ = rutas_escenarios[id_]
    escenario.nombre_ =  str_split(ruta_,"/")
    escenario.nombre = escenario.nombre_[[1]][length(escenario.nombre_[[1]])]
    
    #cargar las piezas desde la ruta
    piezas_csv <- list.files(ruta_, pattern = "\\.csv")
    for (i in seq(1,length(piezas_csv))) {
      pieza_nombre_archivo <- str_split(piezas_csv[i],"\\.")[[1]][1]
      pieza_ <- read.csv(paste0(ruta_,"/",piezas_csv[i]))
      assign(pieza_nombre_archivo, pieza_)
    }
    
    #dividir la plantilla en piezas
    id_arch_ <- c("# stems", "# foliage", "# branches", "# roots", "# mortality table", "# thinning and harvest table" )
    nombre_etiqueta <- plantilla_escenario_CO2Fix %>% select(columna_texto, etiqueta, orden_trozos)
    split_df <- split(nombre_etiqueta, nombre_etiqueta$orden_trozos)
    
    #etiquetas de cada pieza, para localizar los que hay que cambiar
    df_pieza_etiqueta = c()
    df_pieza_orden = c()
    for(i in seq(1, length(split_df))) {
      df_pieza_orden[i] = i
      df_pieza_etiqueta[i] = split_df[[i]][["etiqueta"]][[1]]
    }
    
    #cambiar las piezas en la plantilla
    for (j in seq(1, length(id_arch_))) {
      #etiqueta de la pieza
      etiqueta_ = paste0("\t\t\t\t",id_arch_[j])
      #orden dela pieza en la lista split_df
      orden = df_pieza_orden[df_pieza_etiqueta %in% etiqueta_]
      #archivo 
      arch_n <- get(paste0("archiv_",id_arch_[j])) %>%
        mutate(orden_trozos = split_df[[orden]][["orden_trozos"]][[1]])
      #cambiar la pieza de la plantilla por la pieza cargada
      split_df[[orden]] <- arch_n
    }
    
    #compone el escenario con las piezas en orden
    escenario <- data.frame()
    for (i in unique(nombre_etiqueta$orden_trozos)) {
      i = as.character(i)
      escenario <- escenario %>% bind_rows(split_df[[i]])
    }
    
    escenario <- escenario %>%
      select(columna_texto, etiqueta, orden_trozos)
    
    #Cambiar el nommbre del escenario
    escenario$columna_texto <- str_replace(string = escenario$columna_texto, pattern = "Q1_Rf 120", replacement = escenario.nombre)
    escenario$columna_texto <- str_replace(string = escenario$columna_texto, pattern = "Q1 Selvicultura referencia", replacement = escenario.nombre)
    escenario$columna_texto <- str_replace(escenario$columna_texto,"NaN", "0")
    return(escenario)
  }
  
  #escenarios unidos
  escenarios_unidos = map_dfr(seq(1, length(rutas_escenarios)), ~ funcion_compone_escenario(id_ = .[1]))
  
  #unir los escenarios con el encabezamiento general
  #actualizar el número de escenarios
  n_esc_ori <- "7\t# number of Scenarios"
  n_esc_nuevo <- paste0(as.character(length(rutas_escenarios)),"\t# number of Scenarios")
  #poner a 0 el escenario de mitigación (se escogerá en CO2Fix)
  n_mit_ori <- "6\t# mitigation scenario"
  n_mit_nuevo <- "0\t# mitigation scenario"
  encabezamiento_general_CO2Fix <- str_replace(string = encabezamiento_general_CO2Fix, pattern = n_esc_ori, replacement = n_esc_nuevo)
  encabezamiento_general_CO2Fix <- str_replace(string = encabezamiento_general_CO2Fix, pattern = n_mit_ori, replacement = n_mit_nuevo)
  
  #unir encabezamiento general y escenarios
  salida_ <- data.frame(columna_texto = encabezamiento_general_CO2Fix) %>%
    bind_rows(escenarios_unidos %>% select(columna_texto)) %>%
    select(columna_texto)
  writeLines(salida_$columna_texto, con = paste0("datos/escrib_CO2fix/", archiv_para_CO2Fix))
  
}

map(lista_$lista_var, ~ funcion_montar_CO2Fix_reg_a_irreg(csv_nombre = .[3], densidades_inicial = .[2] ) )



