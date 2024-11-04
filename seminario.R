
install.packages("climaemet")
library(pxR)
library(climaemet)
library(dplyr)


## Use this function to register your API Key temporarly or permanently
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJmcGFibG81NDVAZ21haWwuY29tIiwianRpIjoiYTI2NWIyNzQtY2M4OS00NWZmLThlNGYtMWJlYWQ2NTA1MTAxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE3MjkzNTMxNTgsInVzZXJJZCI6ImEyNjViMjc0LWNjODktNDVmZi04ZTRmLTFiZWFkNjUwNTEwMSIsInJvbGUiOiIifQ.TGFw-QlkAkMyQ2hItIpbSF_xDIoN42JpIzUhf-dOm3A", install= TRUE)




#-----------------------------------------
# DATOS AEMET
#-----------------------------------------

#codigo para extrar todos los datos climaticos de las estaciones de la aemet
estaciones <- aemet_stations()

#obtener las estaciones principales (varias por provincia)
estaciones_ppales <- dplyr::filter(.data = estaciones, nchar(indsinop) > 2)
provincias <- attributes(table(estaciones_ppales$provincia))$dimnames[[1]]
provincias <- provincias[provincias !='BALEARES']

#obtiene las estaciones que contienen el nombre de la provincia
cosa2 <- dplyr::filter(.data = estaciones_ppales,  mapply(grepl, estaciones_ppales$provincia, estaciones_ppales$nombre))

#códigos de estaciones que no interesan
eliminar <- c('C659M', '0201D', '1387D', '1387E', '2867', '3168C', '8175', '8019', '5514', '5514Z', '3168', '3168Z', '3194', '3194U', '3195', '3196', '2539', '9434C', '9434P')
for (i in eliminar){
  cosa2 <- cosa2[!(cosa2$indicativo == i),]
}
resumen <- factor(cosa2$provincia, levels = provincias)
table(resumen)

#ver las que faltan para añadirlas a mano
añadir <- c('6325O', '9091R', '2444', 'B278', '1082', '3469A', '5973', '1111', '8500A', '5402', '1014A', '5270B', '9170', '6156X', '9263D',' 2374X', '0016A', '2661')
for (i in añadir){
  cosa2 <- rbind(cosa2,estaciones_ppales[estaciones_ppales$indicativo == i,])
}
resumen <- factor(cosa2$provincia, levels = provincias)
table(resumen)
cosa2 <- rbind(cosa2, estaciones_ppales[estaciones_ppales$indicativo == '2235U',])
resumen <- factor(cosa2$provincia, levels = provincias)
table(resumen)

#OBTENER LOS CÓDIGOS DE LAS ESTACIONES
codigos <- cosa2$indicativo
datos_met <- data.frame(matrix(ncol = 5))
colnames(datos_met) <- c('codigo', 'ta_max', 'ta_min', 'tm_mes', 'año')
met_22 <- as.data.frame(aemet_monthly_clim('B278', year = 1990)) %>%
  slice (1:12) %>%
  select(c('ta_max', 'ta_min', 'tm_mes'))


met_22$ta_max <- sapply(met_22$ta_max, FUN = function(x) unlist(strsplit(x, '\\('))[1] )
met_22$ta_min <- sapply(met_22$ta_min, FUN = function(x) unlist(strsplit(x, '\\('))[1] )

for (i in codigos){
  print(i)
  met_prov <- as.data.frame(aemet_monthly_clim(i, year = 2000)) %>% 
    slice (1:12) %>% 
    select(c('ta_max', 'ta_min', 'tm_mes'))
  
  met_prov$ta_max <- sapply(met_prov$ta_max, FUN = function(x) as.numeric(unlist(strsplit(x, '\\('))[1]) )
  met_prov$ta_min <- sapply(met_prov$ta_min, FUN = function(x) as.numeric(unlist(strsplit(x, '\\('))[1]) )
  
  
  met_procesados <-  data.frame('codigo' = i,
                                'ta_max' = max(met_prov$ta_max, na.rm = TRUE), 
                                'ta_min' = min(met_prov$ta_min, na.rm = TRUE), 
                                'tm_mes' = mean(met_prov$tm_mes, na.rm = TRUE),
                                'año' = 2000)
  print(met_procesados)
  datos_met <- rbind(datos_met, met_procesados)
  print(datos_met)
  Sys.sleep(1.5)
  
}

#----------------------------------------
# DATOS DIABETES
#----------------------------------------
#obtiene los nombres de archivos
archivos <- list.files('INPUT/DATA/Diabetes/Ingresos/', pattern = '*.px')
#añade la ruta del archivo
archivos_pc <- sapply(archivos, function(x) paste0('INPUT/DATA/Diabetes/Ingresos/',x))

#crea un dataframe con el primer archivo que sevirá de molde para añadir los siguientes años
df_i <- data.frame(as.data.frame(read.px(archivos_pc[1])))
#añade el atributo del año de los datos
df_i['Año'] = 1997

for (i in 2:length(archivos_pc)){
  #genera un df con los datos del año
  df_provisional <- data.frame(as.data.frame(read.px(archivos_pc[i])))
  
  #obtiene el año a partir del nombre del archivo
  año <- unlist(strsplit(archivos[i], "\\."))[1]
  año <- substr(año,nchar(año)-3,nchar(año))
  
  #genera una columna con el año de los datos
  df_provisional['Año'] = año
  
  #hace que los nombres de las columnas sean iguales al dataframe finak
  colnames(df_provisional) <- colnames(df_i)
  
  #une el dataframe con los datos del año al df final
  df_i <- rbind(df_i,df_provisional)
}

#dataframe con todos los datos de todos los años
#df_i
head(df_i)



#Lo mismo para los datos de muertes
datos_muertes <- read.px('INPUT/DATA/Diabetes/Muertes/muertes1997-2022.px')
datos_muertes
df_m <- as.data.frame(datos_muertes)
head(df_m)




