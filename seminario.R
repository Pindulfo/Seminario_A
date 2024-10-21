
install.packages("climaemet")
library(pxR)
library(climaemet)



## Use this function to register your API Key temporarly or permanently
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJmcGFibG81NDVAZ21haWwuY29tIiwianRpIjoiYTI2NWIyNzQtY2M4OS00NWZmLThlNGYtMWJlYWQ2NTA1MTAxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE3MjkzNTMxNTgsInVzZXJJZCI6ImEyNjViMjc0LWNjODktNDVmZi04ZTRmLTFiZWFkNjUwNTEwMSIsInJvbGUiOiIifQ.TGFw-QlkAkMyQ2hItIpbSF_xDIoN42JpIzUhf-dOm3A", install= TRUE)


#codigo para extrar todos los datos climaticos de las estaciones de la aemet

estaciones <- aemet_stations()

# Verificar las primeras estaciones para confirmar la información
head(estaciones)

# Crear un dataframe vacío para almacenar los datos de todas las estaciones
estaciones_unicas_provincia <- estaciones[!duplicated(estaciones$provincia), ]

# Crear un dataframe vacío para almacenar los datos de todas las estaciones
datos_unica_estacion_por_provincia <- data.frame()

# Recorrer las estaciones únicas por provincia
for (i in 1:nrow(estaciones_unicas_provincia)) {
  
  # Obtener el código de cada estación (indicativo)
  codigo_estacion <- estaciones_unicas_provincia[i, "indicativo"]
  
  # Intentar obtener los datos meteorológicos recientes de la estación
  tryCatch({
    datos_clima <- aemet_last_obs(codigo_estacion)
    
    # Agregar los datos al dataframe general
    datos_unica_estacion_por_provincia <- rbind(datos_unica_estacion_por_provincia, datos_clima)
    
    # Imprimir la estación procesada (para seguimiento)
    print(paste("Datos obtenidos para la estación en la provincia de:", estaciones_unicas_provincia[i, "provincia"]))
  },
  error = function(e) {
    # En caso de error, mostrar un mensaje sin detener el ciclo
    print(paste("Error al obtener datos para la estación en la provincia de:", estaciones_unicas_provincia[i, "provincia"]))
  })
}

# Ver los datos obtenidos de las estaciones únicas por provincia
head(datos_unica_estacion_por_provincia)


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




