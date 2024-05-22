#Recuperación de las tablas PCRegenera del IFN4 para determinar la densidad del regenerado de haya


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
                   TABLES=c("PCRegenera"),
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
                                        TABLES=c("PCRegenera"),
                                        DROP_VARS=c(),
                                        ORDER_BY=c(),
                                        PWD="admin",
                                        IFN = "Ifn4",
                                        PROVINCIA = .[2]))


#unir en data frame las tablas "PCMayores","PCDatosMap","PCEspParc", "PCParcelas"
obj_ <- ls(pattern = "PCRegenera") #recuperar los objetos por provincias
PCRegenera_Ifn4 <- map_dfr(obj_, ~ get(.[1]))
rm(list = obj_) #eliminar los objetos por provincias


#recuento del número de especies distintas en la parcela, para retener sólo las monoespecíficas
PCReg_espec_distintas_ <- PCRegenera_Ifn4 %>%
  group_by(Provincia, Estadillo, Especie) %>%
  summarise(cuenta_ = n()) %>%
  group_by(Provincia, Estadillo) %>%
  summarise(espec_distintas = n()) %>%
  ungroup()

  #densidad del regenerado de los pies menores, según la definición del IFN2
  PCRegenera_menores_Ifn4_ <- PCRegenera_Ifn4 %>%
    left_join(PCReg_espec_distintas_) %>%
    filter(Especie == 71 & espec_distintas==1 & CatDes == 4) %>%
    mutate(dens_reg = NumPies/(pi*5^2)*10000)
  
  summary(PCRegenera_menores_Ifn4_$dens_reg)
  ggplot(PCRegenera_menores_Ifn4_, aes(x=dens_reg))+geom_boxplot()
  
  #densidad aproximada de todo el regenerado
    # se suponen las siguientes árboles para las clases 1,2 y 3
    clas_reg <- data.frame(CatDes = c(1,2,3), NumPies_ = c(3,10,20))
    
    PCRegenera_Ifn4_ <- PCRegenera_Ifn4 %>%
      left_join(PCReg_espec_distintas_) %>%
      filter(Especie == 71 & espec_distintas==1) %>%
      left_join(clas_reg) %>%
      mutate(NumPies = ifelse(CatDes!= 4, NumPies_, NumPies)) %>%
      mutate(dens_reg = NumPies/(pi*5^2)*10000)
    
    summary(PCRegenera_Ifn4_$dens_reg)
    ggplot(PCRegenera_Ifn4_, aes(x=dens_reg))+geom_boxplot()
  
