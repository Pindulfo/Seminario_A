
install.packages("climaemet")
library(pxR)
library(climaemet)

## Get api key from AEMET
browseURL("https://opendata.aemet.es/centrodedescargas/obtencionAPIKey")

## Use this function to register your API Key temporarly or permanently
aemet_api_key("MY API KEY")


datos <- read.px("DATA/Diabetes/Ingresos/1997.px")
head(datos)
datos$VALUES$Provincia.de.hospitalización
cosa <- as.data.frame(datos)
cosa
head(cosa)

archivos <- list.files('DATA/Diabetes/Ingresos/', pattern = '*.px')
archivos
?sapply
