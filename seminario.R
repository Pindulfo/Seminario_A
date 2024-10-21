
install.packages("climaemet")
library(pxR)
library(climaemet)


<<<<<<< HEAD
=======
#crea un dataframe con el primer archivo que sevirá de molde para añadir los siguientes años
df <- data.frame(as.data.frame(read.px(archivos_pc[1])))
#añade el atributo del año de los datos
df['Año'] = 1997
>>>>>>> parent of a424043 (datos muertes diabetes cargados)

## Use this function to register your API Key temporarly or permanently
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJmcGFibG81NDVAZ21haWwuY29tIiwianRpIjoiYTI2NWIyNzQtY2M4OS00NWZmLThlNGYtMWJlYWQ2NTA1MTAxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE3MjkzNTMxNTgsInVzZXJJZCI6ImEyNjViMjc0LWNjODktNDVmZi04ZTRmLTFiZWFkNjUwNTEwMSIsInJvbGUiOiIifQ.TGFw-QlkAkMyQ2hItIpbSF_xDIoN42JpIzUhf-dOm3A", install= TRUE)


datos <- read.px("DATA/Diabetes/Ingresos/1997.px")
head(datos)
datos$VALUES$Provincia.de.hospitalización
cosa <- as.data.frame(datos)
cosa
head(cosa)

archivos <- list.files('DATA/Diabetes/Ingresos/', pattern = '*.px')
archivos
?sapply

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
  
<<<<<<< HEAD
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

=======
  #genera una columna con el año de los datos
  df_provisional['Año'] = año
  
  #hace que los nombres de las columnas sean iguales al dataframe finak
  colnames(df_provisional) <- colnames(df)
  
  #une el dataframe con los datos del año al df final
  df <- rbind(df,df_provisional)
}

#dataframe con todos los datos de todos los años
df
>>>>>>> parent of a424043 (datos muertes diabetes cargados)




