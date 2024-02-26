#incremento en diámetro cuadrático medio esperado en función de la calidad de
#estación y la densidad
#Para modelizar el incremento diametral individual se agrupan las parcelas que 
#corresponden (probablemente) a una misma calidad de estación
#(probablemente habría que detallar un proceso bayesiano para establecerlo formalmente)

library(tidyverse)


# Parámetros de inicio
# parámetros relación Dg ~ f(N, Ho)
b0 = 54.2762893472351
b1 = -0.33521433955872
b2 = 0.46960192588597


# parámetros relación Dm ~ f(Dg, N, G)
a7 = 0.191406636051764
a8 = -0.000620621044818245
a10 = 0.0166602839868406

# clases de densidad
clas_dens <- quantile(dat_inc$n_ifn3, seq(0,1, length.out =6))

#

# calidad de estación
IS = c(13,16,19,22,25)

# rango de edades
edad_ini <- 5
edad_fin <- 120


edad = seq(edad_ini,edad_fin, by = 5)
# función de clase de edad
func_Ho <- function(clase = 19, edad_ = 100) {
  clase*((1-1/exp(0.02*edad_))/(1-1/exp(0.02*80)))^1.4823
} 

calidad_estacion = 25
rango_inc_Dg <- function(calidad_estacion_ = calidad_estacion) {
  dens = seq(100,3000, by = 100)
  edad
  Ho <- map 
  
}