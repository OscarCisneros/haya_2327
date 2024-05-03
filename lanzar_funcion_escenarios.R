source("scripts/funcion_genera_escenarios.R")

grupo_ <-  "label_bas_carbon" #"go_fagus" #"planes_comarcales_navarra" "selv_macizo_pirenaico"
densidades_iniciales <- c(7000,5000)

csv_nombres <- list.files(paste0("datos/escenarios/",grupo_), pattern = "\\.csv")
csv_nombres <- substr(csv_nombres, 1, nchar(csv_nombres)-4)

lista_ <- expand.grid(grupo_ = grupo_, densidades_iniciales = densidades_iniciales, csv_nombres = csv_nombres, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(lista_var = list(c(grupo_,densidades_iniciales,csv_nombres)))

oo <- map_dfr(lista_$lista_var, ~funcion_genera_escenarios_(grupo_ = .[1], N_ini_ = .[2],escenario.nombre_ = .[3]) )

#write.xlsx(oo, paste0("resultados/simulaciones/",grupo_,"/","resIS25.xlsx"))

#-------------------------------------------------------------------------------
#lanzar escenarios con volumen minimo extraido de 40 m3
source("scripts/funcion_genera_escenarios_volumen_minimo.R")

grupo_ <-  "mortalidad_natural" #"go_fagus" #"planes_comarcales_navarra" "selv_macizo_pirenaico" "CNPF" "label_bas_carbon" "mortalidad_natural"
densidades_iniciales <- c(7000,5000)

csv_nombres <- list.files(paste0("datos/escenarios/",grupo_), pattern = "\\.csv")
csv_nombres <- substr(csv_nombres, 1, nchar(csv_nombres)-4)

lista_ <- expand.grid(grupo_ = grupo_, densidades_iniciales = densidades_iniciales, csv_nombres = csv_nombres, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(lista_var = list(c(grupo_,densidades_iniciales,csv_nombres)))

oo <- map_dfr(lista_$lista_var, ~funcion_genera_escenarios_vol_min(grupo_ = .[1], N_ini_ = .[2],escenario.nombre_ = .[3]) )

write.xlsx(oo, paste0("resultados/simulaciones/",grupo_,"/","res_.xlsx"))

#-------------------------------------------------------------------------------
#lanzar escenarios con volumen minimo extraido de 40 m3 con generación de paso a monte Irregular
source("scripts/funcion_genera_escenarios_volumen_minimo_IRREG.R")
source("scripts/funcion_genera_escenarios_transformacion_desde_REGULAR_par_CO2Fix.R")
source("scripts/funciones_accesorias_transformacion.R")


grupo_ <-  "prueba" #"go_fagus" #"planes_comarcales_navarra" "selv_macizo_pirenaico" "CNPF"
densidades_iniciales <- c(7000,5000)

csv_nombres <- list.files(paste0("datos/escenarios/",grupo_), pattern = "\\.csv")
csv_nombres <- substr(csv_nombres, 1, nchar(csv_nombres)-4)

lista_ <- expand.grid(grupo_ = grupo_, densidades_iniciales = densidades_iniciales, csv_nombres = csv_nombres, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(lista_var = list(c(grupo_,densidades_iniciales, csv_nombres)))

oo <- map_dfr(lista_$lista_var, ~funcion_genera_escenarios_vol_min_IRREG(grupo_ = .[1], N_ini_ = .[2], escenario.nombre_ = .[3]))


write.xlsx(oo, paste0("resultados/simulaciones/",grupo_,"/","res_.xlsx"))

#-------------------------------------------------------------------------------
#lanzar escenarios de mortalidad natural
source("scripts/funcion_genera_escenarios_mortalidad_natural.R")

grupo_ <-  "mortalidad_natural" #"go_fagus" #"planes_comarcales_navarra" "selv_macizo_pirenaico" "CNPF" "label_bas_carbon" "mortalidad_natural"
densidades_iniciales <- c(7000,5000)

csv_nombres <- list.files(paste0("datos/escenarios/",grupo_), pattern = "\\.csv")
csv_nombres <- substr(csv_nombres, 1, nchar(csv_nombres)-4)

lista_ <- expand.grid(grupo_ = grupo_, densidades_iniciales = densidades_iniciales, csv_nombres = csv_nombres, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(lista_var = list(c(grupo_,densidades_iniciales,csv_nombres)))

oo <- map_dfr(lista_$lista_var, ~funcion_genera_escenarios_mortalidad(grupo_ = .[1], N_ini_ = .[2],escenario.nombre_ = .[3]) )

write.xlsx(oo, paste0("resultados/simulaciones/",grupo_,"/","res_.xlsx"))
