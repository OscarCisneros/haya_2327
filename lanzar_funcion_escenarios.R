source("scripts/funcion_genera_escenarios.R")

grupo_ <-  "go_fagus" #"go_fagus" #"planes_comarcales_navarra" "selv_macizo_pirenaico"
densidades_iniciales <- c(7000,5000)

csv_nombres <- list.files(paste0("datos/escenarios/",grupo_), pattern = "\\.csv")
csv_nombres <- substr(csv_nombres, 1, nchar(csv_nombres)-4)

lista_ <- expand.grid(grupo_ = grupo_, densidades_iniciales = densidades_iniciales, csv_nombres = csv_nombres, stringsAsFactors = FALSE) %>%
  rowwise() %>%
  mutate(lista_var = list(c(grupo_,densidades_iniciales,csv_nombres)))

oo <- map_dfr(lista_$lista_var, ~funcion_genera_escenarios_(grupo_ = .[1], N_ini_ = .[2],escenario.nombre_ = .[3]) )

#write.xlsx(oo, paste0("resultados/simulaciones/",grupo_,"/","resIS25.xlsx"))
