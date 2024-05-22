rr <- data.frame(columna = seq(1,10)) %>%
  mutate(roll_ = rollmedian(columna, 5, fill ="extend"))
rr <- data.frame(columna = seq(1,10)) %>%
mutate(roll_mean_beam = coalesce(rollmedian(columna, 
                                           k = 5, fill = NA), columna))

system2(r"{datos/escrib_CO2fix/plan_comarcal_1_1_8_17_dif_dens_IS22_alta_5000.co2}")
ruta <- "datos/escrib_CO2fix/plan_comarcal_1_1_8_17_dif_dens_IS22_alta_5000.co2"
vv <- readLines(ruta)
system(ruta)
shell.exec(r"{C:\Users\oscar\OneDrive\Documents\R_proyectos\2327\datos\escrib_CO2fix\plan_comarcal_1_1_8_17_dif_dens_IS22_alta_5000.co2}")
shell.exec(r"{datos\escrib_CO2fix\plan_comarcal_1_1_8_17_dif_dens_IS22_alta_5000.co2}")
