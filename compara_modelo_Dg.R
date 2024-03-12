#Comparación del modelo Dg ~f(N,Ho), GoFagus vs IFN3

library(openxlsx)
library(tidyverse)

indices_cal <- c(25,22,19,16,13)

funcion_compara_m.Dg <- function(IS_ = 25) {

  # densidad inicial
   N <- c(3000, 2800, 2500, 2000, 1800, 1500,1000,900,800,600,500,400, 300,200,100, 90, 70, 50, 20, 10)
  # calidad de estación
  IS = IS_
  
  # rango de edades
  edad_ini <- 1
  edad_fin <- 100
  
  # parámetros relación Dg ~ f(N, Ho)
  
      # Parámetros de inicio GOFagus
      b0 = 54.2762893472351
      b1 = -0.33521433955872
      b2 = 0.46960192588597
      
      # Parámetros de inicio IFN3
      v0 = 32.29844
      v1 = -0.3054941
      v2 = 0.5663522

  # parámetros relación V ~ f(Dg,Ho,N)
      
      # Parámetros de inicio GOFagus
      b3 = 0.000231164659251752
      b4 = 1.56932077902809
      b5 = 0.97380243087504
      b6 = 0.901458044581267
      
      # Parámetros de inicio IFN3
      v3 = 6.745875e-05
      v4 = 1.968228
      v5 = 0.9354169
      v6 = 0.9193028
      
  edad = seq(edad_ini,edad_fin, by = 5)
  Ho = IS*((1-1/exp(0.02*edad))/(1-1/exp(0.02*80)))^1.4823
  
  df_ <- data.frame(IS = IS, edad = edad, Ho = Ho, N = N) %>%
      mutate(Dg_GoFagus =  b0*N^b1*Ho^b2,
             Dg_IFN3 = v0*N^v1*Ho^v2) %>%
      mutate(V_GoFagus = b3*Dg_GoFagus^b4*Ho^b5*N^b6,
             V_IFN3 = v3*Dg_IFN3^v4*Ho^v5*N^v6)

  ggplot(df_, aes(x= N, y = Dg_GoFagus))+geom_point(col= "blue")+
    geom_point(aes(x= N, y = Dg_IFN3))
  ggplot(df_, aes(x= edad, y = Dg_GoFagus))+geom_point(col= "blue")+
    geom_point(aes(x= edad, y = Dg_IFN3))
  
  ggplot(df_, aes(x= N, y = V_GoFagus))+geom_point(col= "blue")+
    geom_point(aes(x= N, y = V_IFN3))
  ggplot(df_, aes(x= edad, y = V_GoFagus))+geom_point(col= "blue")+
    geom_point(aes(x= edad, y = V_IFN3))
}  