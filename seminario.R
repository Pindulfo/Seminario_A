library(pxR)

datos <- read.px("DATA/Diabetes/Ingresos/1997.px")
head(datos)
datos$VALUES$Provincia.de.hospitalización
cosa <- as.data.frame(datos)
cosa
head(cosa)

archivos <- list.files('DATA/Diabetes/Ingresos/', pattern = '*.px')
archivos
?sapply
