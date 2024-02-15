#modelo de evolución tras clara de Franklin et al., 2009

library(minpack.lm)
library(tidyverse)

#Relación entre volumen del arbol medio y el número de árboles en espesura completa
      #Nmax is number of trees and b is the biomass of the average tree
      volumen_Nmax <- read.csv2("datos/claras/volumen_Nmax.csv")
      lm_log_V_Nmax = lm(log(V_unit) ~ log(N_max), data = volumen_Nmax)
      
      a = coefficients(lm_log_V_Nmax)[[2]]
      k = exp(coefficients(lm_log_V_Nmax)[[1]])

resumen_claras <- read.csv2("datos/claras/resumen_claras.csv")
#los datos de las parcelas de control no se incluyen en los análisis
resumen_claras_1 <- resumen_claras %>%
  filter(Tratamiento != "A")



#Ug: Gross growth rate relative to a closed stand


      #Um: Mortality relative to a closed stand
      lm_log_Um <- lm(log(Um) ~ 0 + log(c), data = resumen_claras_1 %>% mutate(Um = Um+0.001))
      #el resultado de este ajuste no tiene sentido. Los datos de Um estimados en las parcelas de claras
      #son la resta del recuento de árboles entre claras, no es el dato que se precisa
      
      #una aproximación a g_um es la mediana de los datos del artículo
      g_um <- median(c(4.286,8.63,1.806,2.313,2.589,3.933))
      
      
      
      
      