#gráfico de evolución de distribución diamétrica

funcion_evol_dist_D <- function(id) {
  oo <- dist_D_a[[id]] %>%
    mutate(edad = 5*id)
}

ejemp_evol_dist_D <- map_dfr(seq(3,23), ~funcion_evol_dist_D(.[1]))

ggplot(ejemp_evol_dist_D, aes(x=d_, y= n_d, color=as.factor(edad)))+
  geom_bar(stat="identity", position=position_dodge())+
  theme(legend.position = "none")

ggplot(ejemp_evol_dist_D, aes(x=d_, y= n_d))+
  geom_bar(stat="identity", position=position_dodge(),fill = "gold")+facet_wrap(~edad, scales = "free")+
    ggtitle("Evolución de la distribución diamétrica con la edad")+
  theme_light()+
  labs(x = "Diámetro (cm)", y = "N")+
  theme(text = element_text(size = 15))+
  theme(legend.position = "none",
        strip.background=element_rect(colour="black", fill="blue"))
