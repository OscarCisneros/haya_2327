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
      fn.importData.bbdd <- function(MDBPATH, TABLES, DROP_VARS=c(), ORDER_BY=c(), PWD="", IFN = "Ifn4", PROVINCIA ="") {
        
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
        new_tbl_name <- paste0(new_tbl_name,"_", IFN, "_", PROVINCIA)
        
        ## Assign dataframe to environment
        assign(new_tbl_name, df, envir=.GlobalEnv)
      }
      
      # ## Close and remove channel
        close(channel)
        rm(channel)
}
      
      # Function call to load data. Tables on vector "TABLES"
      fn.importData.bbdd(MDBPATH="datos/Ifn4_Navarra.accdb",
                    TABLES=c("PCMayores","PCDatosMap","PCEspParc", "PCParcelas"),
                    DROP_VARS=c(),
                    ORDER_BY=c(),
                    PWD="admin",
                    IFN = "Ifn4",
                    PROVINCIA = "")

      
      
  #Recuperar las bbdd de IFN4 
      #almacernar los nombres las bbdd en formtato accdb y mdb
     bbdd_nombres <- list.files("datos/bbdd_IFN4", pattern = "\\.*db*")
     #nombres sin tipo de archivo
     nomb_1 <- str_split(bbdd_nombres, pattern = "\\.", simplify = TRUE)
     nomb <- nomb_1[,1]
     df_nomb <- data.frame(bbdd_nombres = bbdd_nombres,
                           nomb = nomb) %>%
       rowwise() %>%
       mutate(lista = list(c(bbdd_nombres, nomb))) %>%
       ungroup()
     
     map(df_nomb$lista, ~ fn.importData.bbdd(MDBPATH=paste0("datos/bbdd_IFN4/",.[1]),
                                                TABLES=c("PCMayores","PCDatosMap","PCEspParc", "PCParcelas"),
                                                DROP_VARS=c(),
                                                ORDER_BY=c(),
                                                PWD="admin",
                                                IFN = "Ifn4",
                                                PROVINCIA = .[2]))
     
     
     #unir en data frame las tablas "PCMayores","PCDatosMap","PCEspParc", "PCParcelas"
     obj_ <- ls(pattern = "PCMayores_Ifn4") #recuperar los objetos por provincias
     PCMayores_Ifn4 <- map_dfr(obj_, ~ get(.[1]))
     rm(list = obj_) #eliminar los objetos por provincias
     
     obj_ <- ls(pattern = "PCDatosMap_Ifn4") #recuperar los objetos por provincias
     #se corrige el campo "Hoja50", en algunas bbdd es character y en otras integer
     funcion_PCDatosMap <- function(obj_i = "PCDatosMap_Ifn4_Ifn4_Burgos") {
       df <- get(obj_i)
       df$Hoja50 <- as.character(df$Hoja50)
       return(df)
     }
     PCDatosMap_Ifn4 <- map_dfr(obj_, ~ funcion_PCDatosMap(.[1]))
     rm(list = obj_) #eliminar los objetos por provincias
     
     obj_ <- ls(pattern = "PCEspParc_Ifn4") #recuperar los objetos por provincias
     PCEspParc_Ifn4 <- map_dfr(obj_, ~ get(.[1]))
     rm(list = obj_) #eliminar los objetos por provincias
     
     obj_ <- ls(pattern = "PCParcelas_Ifn4") #recuperar los objetos por provincias
     #se corrigen campos ("Vuelo2", "Pasada1",, "Pasada2") en algunas bbdd es character y en otras integer
     funcion_PCParcelas <- function(obj_i = "PCDatosMap_Ifn4_Ifn4_Burgos") {
       df <- get(obj_i)
       df$Vuelo2 <- as.character(df$Vuelo2)
       df$Pasada1 <- as.character(df$Pasada1)
       df$Pasada2 <- as.character(df$Pasada2)
       return(df)
     }
     PCParcelas_Ifn4 <- map_dfr(obj_, ~ funcion_PCParcelas(.[1]))
     rm(list = obj_) #eliminar los objetos por provincias
   
     
       
      #selección de parcelas en las que el haya representa más de un % del área basimétrica
      ab_limite <- 0.9 #porcentaje del área basimétrica que representa como mínimo el haya
      fcc_limite <- 90 #límite inferior de fracción de cabida cubierta
      #se podría filtrar por forma del árbol para eliminar los trasmochos, pero hay sólo 1
      
      ########### Mortalidad inventariada ----
      #000: pies cortados; 444: pies no encontrados pero probablemente no cortados
      #888: pies muertos
      
      #hay duplicados en PCMayores_Ifn4, p.e. Provincia == 31, Estadillo == 469
      PCMayores_Ifn4 <- PCMayores_Ifn4 %>% distinct()
      
      PCMayores_Ifn4_haya_depura_1 <- PCMayores_Ifn4 %>%
        mutate(Dn = (Dn1+Dn2)/20) %>%
        mutate(factor_exp = case_when(
          Dn < 12.5 ~ 10000/(pi*5^2), 
          Dn >= 12.5 & Dn < 22.5 ~ 10000/(pi*10^2),
          Dn >= 22.5 & Dn < 42.5 ~ 10000/(pi*15^2),
          Dn >= 42.5 ~ 10000/(pi*25^2)
        )) %>%
        rename(Clase = Cla) %>%
        mutate(retener_0 = !(OrdenIf4 %in% c(0,444,555,666,777,888,999))) %>% #se marcan árboles que no se van a contabilizar en las métricas de parcela
        group_by(Provincia, Estadillo, Clase, Subclase, Especie) %>%
        summarise(ab = sum(retener_0*factor_exp*pi*(Dn/200)^2),
                  dgm = sqrt(sum(retener_0*factor_exp*Dn^2)/sum(retener_0*factor_exp)),
                  n = sum(retener_0*factor_exp),
                  n_mort = sum(factor_exp*(OrdenIf4== 444 | OrdenIf4== 888))) %>%
        ungroup() %>%
        na.omit() %>%
        group_by(Provincia, Estadillo, Clase, Subclase) %>%
        mutate(ab_perc = ab/sum(ab)) %>%
        mutate(retener = ifelse(71 %in% Especie,1,0)) %>%
        mutate(ab_haya_perc = (Especie ==71)*ab_perc) %>%
        filter(sum(ab_haya_perc)>= ab_limite) %>%
        ungroup() %>%
        filter(ab_haya_perc==1) %>%
        select(-retener, -ab_haya_perc) 
      
      # PCMayores_Ifn4_haya_depura_1 <- PCMayores_Ifn4 %>%
      #   mutate(Dn = (Dn1+Dn2)/20) %>%
      #   mutate(factor_exp = case_when(
      #     Dn < 12.5 ~ 10000/(pi*5^2), 
      #     Dn >= 12.5 & Dn < 22.5 ~ 10000/(pi*10^2),
      #     Dn >= 22.5 & Dn < 42.5 ~ 10000/(pi*15^2),
      #     Dn >= 42.5 ~ 10000/(pi*25^2)
      #   )) %>%
      #   rename(Clase = Cla) %>%
      #   mutate(retener_0 = !(OrdenIf4 %in% c(0,444,555,666,777,888,999))) %>%
      #   group_by(Provincia, Estadillo, Clase, Subclase, Especie) %>%
      #   summarise(ab = sum(factor_exp*pi*(Dn/200)^2),
      #             dgm = sqrt(sum(factor_exp*Dn^2)/sum(factor_exp)),
      #             n = sum(factor_exp),
      #             n_mort = sum(factor_exp*(OrdenIf4== 444 | OrdenIf4== 888))) %>% # se contabilizan los árboles muertos, proporcionalmente
      #   ungroup() %>%
      #   na.omit() %>%
      #   group_by(Provincia, Estadillo, Clase, Subclase) %>%
      #   mutate(ab_perc = ab/sum(ab)) %>%
      #   mutate(retener = ifelse(71 %in% Especie,1,0)) %>%
      #   mutate(ab_haya_perc = (Especie ==71)*ab_perc) %>%
      #   filter(sum(ab_haya_perc)>= ab_limite) %>%
      #   ungroup() %>%
      #   filter(ab_haya_perc==1) %>%
      #   select(-retener, -ab_haya_perc) 
      #sólo se retienen los valores de haya, puede no ser correcto
      PCMayores_Ifn4_haya_depura_2 <- PCMayores_Ifn4_haya_depura_1 %>%
        filter(Especie == 71) %>%
        #left_join(PCDatosMap_Ifn4 %>% select(Provincia, Estadillo, Clase, CoorX, CoorY, FccTot, FccArb, Huso)) %>%
        left_join(PCDatosMap_Ifn4 %>% distinct(Provincia, Estadillo, Clase, FccArb)) %>% #no se emplea Subclase, es NA
        filter(FccArb >= fcc_limite)
      
        #el filtro por REGULAR retiene sólo 40 parcelas, se mantienen todas juntas
        # left_join(PCEspParc_Ifn4  %>% filter(Especie == 71 & PosEsp == 1) %>% rename(Clase = Cla) %>% distinct(Provincia, Estadillo, Clase, Subclase, FPMasa)) %>%
        # filter(FPMasa != 4)  #sólo parcelas regulares. No cambia, no hay irregulares tras el filtrado previo
        
      ggplot(PCMayores_Ifn4_haya_depura_2, aes(x=log(dgm),y=log(n)))+
        geom_point()
      
      
      ggplot(PCMayores_Ifn4_haya_depura_2, aes(x=dgm,y=n))+
        geom_point()
      
      #ANÁLISIS ELEMINANDO LAS PARCELAS SIN MORTATIDAD DESDE EL ANTERIOR INVENTARIO
      #se eliminan del análisis las parcelas sin mortalidad desde el anterior inventario
      
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
      
      ggplot(PCMayores_Ifn4_haya_depura_2 %>% filter(n_mort > 0), aes(x= dgm, y =n ))+geom_point(size = 4, color = "blue")+
        geom_smooth(stat = "smooth")+
        geom_line(aes(x=dgm, y=exp(predict(cobbDouglas)))) +
        ggtitle("Modelo de mortalidad natural. SFA")+
        scale_x_continuous(breaks = seq(0, 120, by = 5))+
        theme_light()+
        labs(x = "Dg (cm)", y = "N (arb/ha)")+
        theme(text = element_text(size = 30)) 
      
      #comprobación de los valores con datos por encima de la previsión del modelo
      uu <- PCMayores_Ifn4_haya_depura_2 %>%
        filter(n_mort > 0) %>%
        mutate(predicho = exp(predict(cobbDouglas))) %>% filter(predicho < n)
      
      #ANÁLISIS SIN ELIMINAR LAS PARCELAS SIN MORTATIDAD DESDE EL ANTERIOR INVENTARIO
      #se eliminan del análisis las parcelas sin mortalidad desde el anterior inventario
      
      cobbDouglas <- sfa( log(n) ~ log(dgm),
                          data = PCMayores_Ifn4_haya_depura_2 )
      summary(cobbDouglas)
      
      ggplot(PCMayores_Ifn4_haya_depura_2, aes(x= log(dgm), y =log(n) ))+geom_point()+
        geom_line(aes(x=log(dgm), y=predict(cobbDouglas)))
      
      ggplot(PCMayores_Ifn4_haya_depura_2 , aes(x= log(dgm), y =log(n) ))+geom_point(size = 4, color = "blue")+
        geom_line(aes(x=log(dgm), y=predict(cobbDouglas))) +
        ggtitle("Modelo de mortalidad natural. SFA")+
        theme_light()+
        labs(x = "log(Dg)", y = "log(N)")+
        theme(text = element_text(size = 30)) 
      
      ggplot(PCMayores_Ifn4_haya_depura_2 , aes(x= dgm, y =n ))+geom_point(size = 4, color = "blue")+
        geom_smooth(stat = "smooth")+
        geom_line(aes(x=dgm, y=exp(predict(cobbDouglas)))) +
        ggtitle("Modelo de mortalidad natural. SFA")+
        scale_x_continuous(breaks = seq(0, 120, by = 5))+
        theme_light()+
        labs(x = "Dg (cm)", y = "N (arb/ha)")+
        theme(text = element_text(size = 30)) 
      
      
      
      
      
      
      
      # ggplot(PCMayores_Ifn4_haya_depura_2 , aes(x= dgm, y =n_mort/n))+geom_point()
      # 
      # lm.n_mort_dgm <- lm(log(n_mort/n+0.001) ~ log(dgm), data = PCMayores_Ifn4_haya_depura_2)
      # 
      # ggplot(PCMayores_Ifn4_haya_depura_2 , aes(x= dgm, y =n_mort/n))+geom_point()+
      #   geom_line(aes(x=dgm, y= exp(predict(lm.n_mort_dgm))))
      
      # gg <- data.frame(n_ha = PCMayores_Ifn4_haya_depura_2$n,
      #                  dgm = PCMayores_Ifn4_haya_depura_2$dgm,
      #                  limite = exp(predict(cobbDouglas)))


      ################################################################################
      ########### Cargar las tablas del IFN 3 ----
      

      #Recuperar las bbdd de IFN3 
      #almacernar los nombres las bbdd en formtato accdb y mdb
      bbdd_nombres <- list.files("datos/bbdd_IFN3", pattern = "\\.*db*")
      #nombres sin tipo de archivo
      nomb_1 <- str_split(bbdd_nombres, pattern = "\\.", simplify = TRUE)
      nomb <- nomb_1[,1]
      df_nomb <- data.frame(bbdd_nombres = bbdd_nombres,
                            nomb = nomb) %>%
        rowwise() %>%
        mutate(lista = list(c(bbdd_nombres, nomb))) %>%
        ungroup()
      
      map(df_nomb$lista, ~ fn.importData.bbdd(MDBPATH=paste0("datos/bbdd_IFN3/",.[1]),
                                              TABLES=c("PCMayores","PCEspParc", "PCParcelas"),
                                              DROP_VARS=c(),
                                              ORDER_BY=c(),
                                              PWD="admin",
                                              IFN = "Ifn3",
                                              PROVINCIA = .[2]))
      
      
      #unir en data frame las tablas "PCMayores","PCDatosMap","PCEspParc", "PCParcelas"
      obj_ <- ls(pattern = "PCMayores_Ifn3") #recuperar los objetos por provincias
      #se añade el campo Provincia
      funcion_PCMayores_ <- function(obj_i = "PCDatosMap_Ifn4_Ifn4_Burgos") {
        df <- get(obj_i)
        provincia <- str_sub(obj_i,  -2)
        provincia <- as.integer(provincia)
        df$Provincia <- provincia
       
        return(df)
      }
      PCMayores_Ifn3 <- map_dfr(obj_, ~ funcion_PCMayores_(.[1]))
      rm(list = obj_) #eliminar los objetos por provincias
      
      obj_ <- ls(pattern = "PCEspParc_Ifn3") #recuperar los objetos por provincias
      #se corrigen campos ("Vuelo2", "Pasada1",, "Pasada2") en algunas bbdd es character y en otras integer
      funcion_PCEspParc <- function(obj_i = "PCDatosMap_Ifn4_Ifn4_Burgos") {
        df <- get(obj_i)
        df$OrgMasa2 <- as.character(df$OrgMasa2)
        provincia <- str_sub(obj_i,  -2)
        provincia <- as.integer(provincia)
        df$Provincia <- provincia
        # df$Pasada1 <- as.character(df$Pasada1)
        # df$Pasada2 <- as.character(df$Pasada2)
        return(df)
      }
      PCEspParc_Ifn3 <- map_dfr(obj_, ~ funcion_PCEspParc(.[1]))
      rm(list = obj_) #eliminar los objetos por provincias
      
      obj_ <- ls(pattern = "PCParcelas_Ifn3") #recuperar los objetos por provincias
      #se eliminan campos para evitar conflictos de tipo de variable y se cambia FechaIni
      #en la bbdd de la RIoja
      funcion_PCParcelas <- function(obj_i = "PCDatosMap_Ifn4_Ifn4_Burgos") {
        df <- get(obj_i)
        df <- df %>% select(Provincia, Estadillo, Cla, Subclase, FccTot, FccArb, FechaIni)
        
        if (obj_i == "PCParcelas_Ifn3_Ifn3p26") {
          df$FechaIni <-  as.Date(df$FechaIni, "%d/%m/%y")
        } else {
        
        df$FechaIni <- as.POSIXct(df$FechaIni)}
    
        return(df)
      }
      PCParcelas_Ifn3 <- map_dfr(obj_, ~ funcion_PCParcelas(.[1]))
      rm(list = obj_) #eliminar los objetos por provincias
      
      
      ################################################################################
      ########### Tiempo entre inventarios ----
      
      
      # Function call to load data. Tables on vector "TABLES"
      PCParcelas_tiempo <- PCParcelas_Ifn4 %>%
        filter(Cla == "A" & Subclase == 1) %>% #parcelas medidas también en el Ifn3
        distinct(Provincia, Estadillo, FechaIni) %>% #hay datos duplicados, p.e. Pronvincia 31, Estadillo 2
        rename(Fecha_Ifn4 = FechaIni) %>%
        left_join(PCParcelas_Ifn3 %>% 
                    filter(Cla == "A" & Subclase == 1) %>% #Para evitar duplicados de fecha, son parcelas bien identificadas y remedidas
                    select(Provincia, Estadillo, FechaIni) %>% rename(Fecha_Ifn3 = FechaIni)) %>%
        mutate(dias = difftime(Fecha_Ifn4, Fecha_Ifn3)) %>%
        mutate(anno = as.numeric(dias)/365)
      
      #hay duplicados en PCMayores_Ifn4, p.e. Provincia == 31, Estadillo == 469
      PCMayores_Ifn3 <- PCMayores_Ifn3 %>% distinct()
      
      PCMayores_Ifn3_haya_depura_1 <- PCMayores_Ifn3 %>%
        mutate(Dn = (Dn1+Dn2)/20) %>%
        mutate(factor_exp = case_when(
          Dn < 12.5 ~ 10000/(pi*5^2), 
          Dn >= 12.5 & Dn < 22.5 ~ 10000/(pi*10^2),
          Dn >= 22.5 & Dn < 42.5 ~ 10000/(pi*15^2),
          Dn >= 42.5 ~ 10000/(pi*25^2)
        )) %>%
        rename(Clase = Cla) %>%
        mutate(retener_0 = !(OrdenIf3 %in% c(0,444,555,666,777,888,999))) %>% #se marcan árboles que no se van a contabilizar en las métricas de parcela
        group_by(Provincia, Estadillo, Clase, Subclase, Especie) %>%
        summarise(ab = sum(retener_0*factor_exp*pi*(Dn/200)^2),
                  dgm = sqrt(sum(retener_0*factor_exp*Dn^2)/sum(retener_0*factor_exp)),
                  n = sum(retener_0*factor_exp)) %>%
        ungroup() %>%
        na.omit() %>%
        group_by(Provincia, Estadillo, Clase, Subclase) %>%
        mutate(ab_perc = ab/sum(ab)) %>%
        mutate(retener = ifelse(71 %in% Especie,1,0)) %>%
        mutate(ab_haya_perc = (Especie ==71)*ab_perc) %>%
        filter(sum(ab_haya_perc)>= ab_limite) %>%
        ungroup() %>%
        filter(ab_haya_perc==1) %>%
        select(-retener, -ab_haya_perc) 
      
      
      PCParcelas_mort <- PCMayores_Ifn4_haya_depura_2 %>%
        select(Provincia, Estadillo, Especie, n_mort) %>%
        left_join(PCMayores_Ifn3_haya_depura_1 %>% select(Provincia, Estadillo, n)) %>%
        left_join(PCParcelas_tiempo) %>% 
        mutate(mort_anual = (n_mort/n)/anno) %>%
        na.omit()
        
      summary(PCParcelas_mort$mort_anual)
      mort_5_ <- 5*mean(PCParcelas_mort$mort_anual)
        
        
        
      ################################################################################
      ########### Mortalidad inventariada ----
      #000: pies cortados; 444: pies no encontrados pero probablemente no cortados
      #888: pies muertos
     #  
     # PCMortalidad_Ifn4 <- PCMayores_Ifn4_haya_depura_2 %>%
     #    filter(Clase == "A" & Subclase == 1) %>% #parcelas medidas también en el Ifn3
     #    mutate(muerto = ifelse())
     # 
     #  ggplot(PCMayores_Ifn4_haya_depura_2, aes(x=dgm, y = (n_mort/n)))+geom_point()
      
# 
# PCEspParc_haya <- PCEspParc %>% filter(Especie == 71) %>% count(Estadillo)
# 
# PCDatosMpa_haya_con_edad <- PCDatosMap %>%
#   left_join(PCEspParc_haya)
# write.csv(PCDatosMpa_haya_con_edad, "PCDatosMpa_haya_con_edad.csv")
