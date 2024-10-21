
install.packages("climaemet")
library(pxR)
library(climaemet)



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





