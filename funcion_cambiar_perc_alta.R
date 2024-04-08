#cambiar los escenarios de GoFagus a alta con porcentajes 0/1

#abrir archivos
csv_alta <- list.files(paste0("datos/escenarios/go_fagus/"), pattern = "alta.csv")
funcion_cambiar_alta_go_fagus <- function(id = 1) {
  H_alta <- read.csv2(paste0("datos/escenarios/go_fagus/", csv_alta[id]))
  H_alta$perc_extrac_baja[H_alta$perc_extrac_baja >= 0] = 0
  H_alta$perc_extrac_alta[H_alta$perc_extrac_alta >= 0] = 1
  write.csv2(H_alta, paste0("datos/escenarios/go_fagus/",csv_alta[id]))
}
map(seq(1,length(csv_alta)), ~ funcion_cambiar_alta_go_fagus(id= .[1]))
