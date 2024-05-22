#cálculo de tarifa de volumen
#a partir de la ecuación de volumen del IFN4
# VCC = a + b (D.n.)²  H.t. (VCC en dm3, Dn en mm, Ht en m)


param_ecuacion_volumen_fs_IFN4 <- read.csv("datos/param_ecuacion_volumen_fs_IFN4.csv", sep=";")
Pares_Dn_Vol <- PCMayores_Ifn4 %>%
  filter(Especie == 71 & Provincia == 31 & Forma <= 3) %>%
  mutate(Dn_mm = (Dn1+Dn2)/2) %>%
  left_join(param_ecuacion_volumen_fs_IFN4 %>% select(Forma, a, b)) %>%
  select(Dn_mm, Ht, a, b, Forma) %>%
  na.omit() %>%
  mutate(Vcc = a+b*(Dn_mm)^2*Ht)

ggplot(Pares_Dn_Vol, aes(x=Dn_mm, y = Vcc, col = as.factor(Forma)))+geom_point()

lm.vcc <- lm(Vcc ~ Dn_mm + I(Dn_mm^2), data = Pares_Dn_Vol)  
summary(lm.vcc)
save(lm.vcc, file = "datos/lm.vcc")

jj <- data.frame(Dn_mm = seq(50,750, by=50)) %>%
  mutate(predicho = predict(lm.vcc, newdata = data.frame(Dn_mm = Dn_mm))/1000)
ggplot(jj, aes(x= Dn_mm, y = predicho))+geom_point()

#estadísticos de ajuste
ajuste_vol <- data.frame(observado = Pares_Dn_Vol$Vcc,
                         predicho = predict(lm.vcc),
                         residuos = resid(lm.vcc)) 

#Estadísticos
estad_ajuste_vol <- data.frame(
  modelo = "dmin",
  SSE = round(sum((ajuste_vol$residuos)^2),1),
  SST = round(sum((Pares_Dn_Vol$Vcc-mean(Pares_Dn_Vol$Vcc))^2),1),
  nparms = 3) %>%
  mutate(
    e = round(mean(ajuste_vol$residuos),4),
    R2 = round((1-SSE/SST)*100,2),
    RMCE = round(sqrt(SSE/(nrow(Pares_Dn_Vol)-nparms)),3)) %>%
  mutate(e_perc = round(e*100/mean(Pares_Dn_Vol$Vcc),4),
         RMCE_perc = round(RMCE*100/mean(Pares_Dn_Vol$Vcc),3)) %>%
  select(modelo, e, e_perc, RMCE, RMCE_perc, R2)
