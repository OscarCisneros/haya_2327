library(RODBC)
library(frontier)
library(tidyverse)



## Set up driver info and database path
# DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
# MDBPATH <- "datos/Ifn4_Navarra.accdb"  #"datos/Sig_Navarra.accdb"
# PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

## Set up driver info and database path
# DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
# MDBPATH <- "datos/Sig_Navarra.accdb" #"datos/Ifn4_Navarra.accdb"  
# PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH)

## Establish connection
# channel <- odbcDriverConnect(PATH)
# sqlTables(channel, tableType = "TABLE")
# sqlColumns(channel, "PCDatosMap")

################################################################################
########### Cargar las tablas del IFN 4 ----

## Generic function for loading data
      fn.importData <- function(MDBPATH, TABLES, DROP_VARS=c(), ORDER_BY=c(), PWD="", IFN = "Ifn4") {
        
        ## Set up driver info and database path
        library(RODBC)
        DRIVERINFO <- "Driver={Microsoft Access Driver (*.mdb, *.accdb)};"
        PATH <- paste0(DRIVERINFO, "DBQ=", MDBPATH, ";PWD=", PWD)
        
        ## Establish connection
        channel <- odbcDriverConnect(PATH)
        
        ## Iterate through list of tables
        for (tbl in TABLES) {
          
          ## Retrieve all variable names from table tbl
          tbl_vars <- sqlColumns(channel, tbl)["COLUMN_NAME"]
          ## Exclude variables based on input parameters
          tbl_vars <- subset(tbl_vars, !(tbl_vars$COLUMN_NAME %in% DROP_VARS))
          ## Add brackets to each variable (ie. [variable]) to maintain ACCESS syntax
          tbl_vars$COLUMN_NAME <- paste0("[", tbl_vars$COLUMN_NAME, "]")
          ## Transform dataframe column into string separated by comma
          cols <- paste0(tbl_vars[1:nrow(tbl_vars), ], collapse=",")
          ## Create ORDER BY string
          if (length(ORDER_BY) > 0) {
            order <- paste0("ORDER BY", paste0(paste0("[", ORDER_BY, "]"), collapse=", "))
          }
          order <- ""
        
        ## Extract table of interest as dataframe
        df <- sqlQuery(channel,
                       paste0("SELECT ", cols, " FROM [", tbl, "]", order, ";"),
                       stringsAsFactors=FALSE)
        
        ## Replace dash with underscore
        new_tbl_name <- gsub("-", "_", tbl)
        new_tbl_name <- paste0(new_tbl_name,"_", IFN)
        
        ## Assign dataframe to environment
        assign(new_tbl_name, df, envir=.GlobalEnv)
      }
      
      # ## Close and remove channel
        close(channel)
        rm(channel)
}
      
      # Function call to load data. Tables on vector "TABLES"
      fn.importData(MDBPATH="datos/Ifn4_Navarra.accdb",
                    TABLES=c("PCMayores","PCDatosMap","PCEspParc", "PCParcelas"),
                    DROP_VARS=c(),
                    ORDER_BY=c(),
                    PWD="admin",
                    IFN = "Ifn4")

#selección de parcelas en las que el haya representa más del 80% del área basimétrica
      ab_limite <- 0.9 #porcentaje del área basimétrica que representa como mínimo el haya
      fcc_limite <- 90 #límite inferior de fracción de cabida cubierta
      #se podría filtrar por forma del árbol para eliminar los trasmochos, pero hay sólo 1
      
      ########### Mortalidad inventariada ----
      #000: pies cortados; 444: pies no encontrados pero probablemente no cortados
      #888: pies muertos
      
      PCMayores_Ifn4_haya_depura_1 <- PCMayores_Ifn4 %>%
        mutate(Dn = (Dn1+Dn2)/20) %>%
        mutate(factor_exp = case_when(
          Dn < 12.5 ~ 10000/(pi*5^2), 
          Dn >= 12.5 & Dn < 22.5 ~ 10000/(pi*10^2),
          Dn >= 22.5 & Dn < 42.5 ~ 10000/(pi*15^2),
          Dn >= 42.5 ~ 10000/(pi*25^2)
        )) %>%
        group_by(Provincia, Estadillo, Especie) %>%
        summarise(ab = sum(factor_exp*pi*(Dn/200)^2),
                  dgm = sqrt(sum(factor_exp*Dn^2)/sum(factor_exp)),
                  n = sum(factor_exp),
                  n_mort = sum(factor_exp*(OrdenIf4== 444 | OrdenIf4== 888))) %>% # se contabilizan los árboles muertos, proporcionalmente
        ungroup() %>%
        na.omit() %>%
        group_by(Estadillo) %>%
        mutate(ab_perc = ab/sum(ab)) %>%
        mutate(retener = ifelse(71 %in% Especie,1,0)) %>%
        mutate(ab_haya_perc = (Especie ==71)*ab_perc) %>%
        filter(sum(ab_haya_perc)>= ab_limite) %>%
        ungroup() %>%
        filter(ab_haya_perc==1) %>%
        select(-retener, -ab_haya_perc) 
      #sólo se retienen los valores de haya, puede no ser correcto
      PCMayores_Ifn4_haya_depura_2 <- PCMayores_Ifn4_haya_depura_1 %>%
        filter(Especie == 71) %>%
        left_join(PCDatosMap_Ifn4 %>% select(Provincia, Estadillo, CoorX, CoorY, FccTot, FccArb, Huso)) %>%
        filter(FccArb >= fcc_limite)
        
        
      ggplot(PCMayores_Ifn4_haya_depura_2, aes(x=log(dgm),y=log(n)))+
        geom_point()
      
      #se eliminan del análisis las parcelas sin mortalidad desde el anterior inventario
      #
        
      cobbDouglas <- sfa( log(n) ~ log(dgm),
                          data = PCMayores_Ifn4_haya_depura_2 %>% filter(n_mort > 0) )
      summary(cobbDouglas)
      
      ggplot(PCMayores_Ifn4_haya_depura_2 %>% filter(n_mort > 0), aes(x= log(dgm), y =log(n) ))+geom_point()+
        geom_line(aes(x=log(dgm), y=predict(cobbDouglas)))
        
      ggplot(PCMayores_Ifn4_haya_depura_2 %>% filter(n_mort > 0), aes(x= log(dgm), y =log(n) ))+geom_point(size = 4, color = "blue")+
        geom_line(aes(x=log(dgm), y=predict(cobbDouglas))) +
        ggtitle("Modelo de mortalidad natural. SFA")+
        theme_light()+
        labs(x = "log(Dg)", y = "log(N)")+
        theme(text = element_text(size = 30)) 
      
      # ggsave("resumen_resultados/evaluacion_Test_Bi.png", width = 677.4 , height = 364.416, units = "mm")
      
      
      ggplot(PCMayores_Ifn4_haya_depura_2 , aes(x= dgm, y =n_mort/n))+geom_point()
      
      lm.n_mort_dgm <- lm(log(n_mort/n+0.001) ~ log(dgm), data = PCMayores_Ifn4_haya_depura_2)
      
      ggplot(PCMayores_Ifn4_haya_depura_2 , aes(x= dgm, y =n_mort/n))+geom_point()+
        geom_line(aes(x=dgm, y= exp(predict(lm.n_mort_dgm))))
      
      gg <- data.frame(n_ha = PCMayores_Ifn4_haya_depura_2$n,
                       dgm = PCMayores_Ifn4_haya_depura_2$dgm,
                       limite = exp(predict(cobbDouglas)))


      ################################################################################
      ########### Cargar las tablas del IFN 3 ----
      

      # Function call to load data. Tables on vector "TABLES"
      fn.importData(MDBPATH="datos/Ifn3p31.accdb",
                    TABLES=c("PCParcelas","PCMayores"),
                    DROP_VARS=c(),
                    ORDER_BY=c(),
                    PWD="admin",
                    IFN = "Ifn3")
      
      
      ################################################################################
      ########### Tiempo entre inventarios ----
      
      
      # Function call to load data. Tables on vector "TABLES"
      PCParcelas_tiempo <- PCParcelas_Ifn4 %>%
        filter(Cla == "A" & Subclase == 1) %>% #parcelas medidas también enn el Ifn3
        select(Provincia, Estadillo, FechaIni) %>%
        rename(Fecha_Ifn4 = FechaIni) %>%
        left_join(PCParcelas_Ifn3 %>% select(Provincia, Estadillo, FechaIni) %>% rename(Fecha_Ifn3 = FechaIni)) %>%
        mutate(dias = difftime(Fecha_Ifn4, Fecha_Ifn3)) %>%
        mutate(anno = as.numeric(dias)/365)
      
      PCParcelas_mort <- PCMayores_Ifn4_haya_depura_2 %>%
        select(Provincia, Estadillo, Especie, n, n_mort) %>%
        left_join(PCParcelas_tiempo) %>% 
        mutate(mort_anual = (n_mort/n)/anno) %>%
        na.omit()
        
      summary(PCParcelas_mort$mort_anual)
      mort_5_ <- 5*mean(PCParcelas_mort$mort_anual)
        
        
        
      ################################################################################
      ########### Mortalidad inventariada ----
      #000: pies cortados; 444: pies no encontrados pero probablemente no cortados
      #888: pies muertos
      
     PCMortalidad_Ifn4 <- PCMayores_Ifn4_haya_depura_2 %>%
        filter(Cla == "A" & Subclase == 1) %>% #parcelas medidas también en el Ifn3
        mutate(muerto = ifelse())

      ggplot(PCMayores_Ifn4_haya_depura_2, aes(x=dgm, y = (n_mort/n)))+geom_point()
      
# 
# PCEspParc_haya <- PCEspParc %>% filter(Especie == 71) %>% count(Estadillo)
# 
# PCDatosMpa_haya_con_edad <- PCDatosMap %>%
#   left_join(PCEspParc_haya)
# write.csv(PCDatosMpa_haya_con_edad, "PCDatosMpa_haya_con_edad.csv")
