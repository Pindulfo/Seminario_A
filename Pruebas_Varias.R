#----------------------------------------------
#Paquetes necesarios
#----------------------------------------------
install.packages("climaemet")
library(pxR)
library(climaemet)
library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
library(plotly)

## Use this function to register your API Key temporarly or permanently
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJmcGFibG81NDVAZ21haWwuY29tIiwianRpIjoiYTI2NWIyNzQtY2M4OS00NWZmLThlNGYtMWJlYWQ2NTA1MTAxIiwiaXNzIjoiQUVNRVQiLCJpYXQiOjE3MjkzNTMxNTgsInVzZXJJZCI6ImEyNjViMjc0LWNjODktNDVmZi04ZTRmLTFiZWFkNjUwNTEwMSIsInJvbGUiOiIifQ.TGFw-QlkAkMyQ2hItIpbSF_xDIoN42JpIzUhf-dOm3A", install= TRUE)

#-----------------------------------------
# DATOS AEMET
#-----------------------------------------

#codigo para extrar todos los datos climaticos de las estaciones de la aemet
estaciones <- aemet_stations()

estaciones <- estaciones %>% 
  select(indicativo,provincia) %>% 
  rename(codigo = indicativo)
typeof(datos_met$codigo)
typeof(estaciones$codigo)

datos_met <- merge(datos_met,estaciones, by = 'codigo')

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

for (año in 2017:2020){
  print(año)
  for (i in codigos){
    print(i)
    met_prov <- as.data.frame(aemet_monthly_clim(i, year = año)) %>% 
      slice (1:12) %>% 
      select(c('ta_max', 'ta_min', 'tm_mes'))
    
    met_prov$ta_max <- sapply(met_prov$ta_max, FUN = function(x) as.numeric(unlist(strsplit(x, '\\('))[1]) )
    met_prov$ta_min <- sapply(met_prov$ta_min, FUN = function(x) as.numeric(unlist(strsplit(x, '\\('))[1]) )
    
    
    met_procesados <-  data.frame('codigo' = i,
                                  'ta_max' = max(met_prov$ta_max, na.rm = TRUE), 
                                  'ta_min' = min(met_prov$ta_min, na.rm = TRUE), 
                                  'tm_mes' = mean(met_prov$tm_mes, na.rm = TRUE),
                                  'año' = año)
    # print(met_procesados)
    datos_met <- rbind(datos_met, met_procesados)
    # print(datos_met)
    Sys.sleep(2)
  }
}

save(datos_met,file = './datos_aemet.RData')

#-------------------------------------------------------
#Modificacion del dataframe de datos_aemet
#-------------------------------------------------------

#Carga de archivos
load(file = 'objetos.RData')
load(file = 'datos_aemet.RData')
cod_estaciones <- estaciones %>% 
  select(indicativo,provincia) %>% 
  rename(codigo = indicativo)
typeof(datos_met$codigo)
typeof(cod_estaciones$codigo)

#     Ejemplo de gráficos:

library(ggplot2)
library(plotly)

# Filtrar y preparar datos climáticos
datos_met_filtrados <- datos_met %>% filter(!is.na(año))

# Crear el gráfico
grafico_temperatura <- ggplot(datos_met_filtrados, aes(x = año, y = tm_mes, group = codigo, color = codigo)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Temperatura Promedio por Año",
    x = "Año",
    y = "Temperatura Media (°C)",
    color = "Estación"
  ) +
  theme_minimal()

# Mostrar con ggplot
print(grafico_temperatura)

# Convertir a gráfico interactivo con plotly
grafico_interactivo <- ggplotly(grafico_temperatura)
grafico_interactivo

#   Gráfico 2:

# Preparar datos de ingresos por diabetes
df_i_filtrados <- df_i %>%
  filter(!is.na(Año), Sexo %in% c("Hombres", "Mujeres")) %>%
  group_by(Año, Sexo) %>%
  summarise(Ingresos = sum(Ingresos, na.rm = TRUE))

# Crear el gráfico
grafico_diabetes <- ggplot(df_i_filtrados, aes(x = as.numeric(Año), y = Ingresos, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Ingresos por Diabetes por Año y Sexo",
    x = "Año",
    y = "Ingresos",
    fill = "Sexo"
  ) +
  theme_minimal()

# Mostrar con ggplot
print(grafico_diabetes)

# Convertir a gráfico interactivo con plotly
grafico_interactivo_diabetes <- ggplotly(grafico_diabetes)
grafico_interactivo_diabetes

#  Gráfico 3:

# Preparar datos de ingresos por provincia
df_i_provincia <- df_i %>%
  group_by(Provincia.de.hospitalización) %>%
  summarise(Total_Ingresos = sum(Ingresos, na.rm = TRUE))

# Crear el gráfico
grafico_provincias <- ggplot(df_i_provincia, aes(x = reorder(Provincia.de.hospitalización, -Total_Ingresos), y = Total_Ingresos)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Ingresos por Diabetes por Provincia",
    x = "Provincia",
    y = "Ingresos Totales"
  ) +
  theme_minimal()

# Mostrar con ggplot
print(grafico_provincias)

