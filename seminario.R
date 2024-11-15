
install.packages("climaemet")
library(pxR)
library(climaemet)
library(dplyr)
library(stringr)

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

#----------------
# Pruebas

for (i in 2000:2020){
  print(i)
  prueba <- as.data.frame(aemet_monthly_clim('6155A', year = i)) %>%
    slice (1:12) %>%
    select(c('ta_max', 'ta_min', 'tm_mes'))
  Sys.sleep(1.5)
}
print(codigos)


mal <-  c('C659H','3168D','9091R','1014A','6156X')
bien <- c('C658L','3013','9091O','1014','6155A')
remplazo <- c('C659H' = 'C658L', '3168D' = '3013','9091R'= '9091O', '1014A' = '1014', '6156X' = '6155A')

codigos <- str_replace_all(codigos, remplazo)

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

# dataframe con todos los datos de ingresos de todos los años
df_i
df_i <- select(df_i,-Diagnóstico)

head(df_i)


# análisis de los valores/niveles de cada atributo
length(levels(factor(df_i$Provincia.de.hospitalización)))

#Quita los numeros de la provincia | 
df_i$Provincia.de.hospitalización <- str_replace_all(df_i$Provincia.de.hospitalización,"[0123456789]","") %>% 
  str_replace_all("^ ","") %>% 
  str_replace_all("Araba/Á","A") %>% 
  str_replace_all("/Alacant", '') %>% 
  str_replace_all("ANDALUCÍA", 'Andalucía') %>% 
  str_replace_all('ARAGÓN', 'Aragón') %>% 
  str_replace_all('ASTURIAS','Asturias') %>% 
  str_replace_all('Asturias \\(Principado de\\)', 'Asturias') %>%
  str_replace_all('Asturias, Principado de', 'Asturias') %>%  
  str_replace_all('Asturias, PRINCIPADO DE', 'Asturias') %>%
  str_replace_all('Ávila', 'Avila') %>% 
  str_replace_all('BALEARS, ILLES', 'Baleares') %>% 
  str_replace_all('Balears, Illes', 'Baleares') %>% 
  str_replace_all('Balears \\(Illes\\)', 'Baleares') %>% 
  str_replace_all("CANARIAS", 'Canarias') %>% 
  str_replace_all("CANTABRIA", 'Cantabria') %>% 
  str_replace_all("Castellón de la Plana", 'Castellón') %>% 
  str_replace_all("/Castelló", '') %>% 
  str_replace_all("CASTILLA - LA MANCHA", 'Castilla - La Mancha') %>% 
  str_replace_all("CASTILLA Y LEÓN", 'Castilla y León') %>% 
  str_replace_all("CATALUÑA", 'Cataluña') %>% 
  str_replace_all("Comunitat Valenciana", 'Comunidad Valenciana') %>% 
  str_replace_all("COMUNITAT VALENCIANA", 'Comunidad Valenciana') %>% 
  str_replace_all("Coruña \\(A\\)", 'A Coruña') %>% #revisar el Coruña (A) no se ha modificado con esto
  str_replace_all("Coruña, A", 'A Coruña') %>% 
  str_replace_all("EXTREMADURA", 'Extremadura') %>% 
  str_replace_all("GALICIA", 'Galicia') %>% 
  str_replace_all("Guipúzcoa", 'Gipuzkoa') %>% 
  str_replace_all('MADRID, COMUNIDAD DE','Madrid') %>% #revisar al igual que asturias para que solo salga Madrid
  str_replace_all('Madrid, Comunidad de', 'Madrid') %>%
  str_replace_all('Madrid \\(Comunidad de\\)', 'Madrid')
  str_replace_all('MURCIA, REGIÓN DE','Murcia') %>% #revisar para que solo salga Murcia
  str_replace_all('Murcia \\(Región de\\)','Murcia') %>%
  str_replace_all('Murcia, Región de','Murcia') %>%
  str_replace_all('Murcia*', 'Murcia') %>% 
  str_replace_all('NAVARRA, COMUNIDAD FORAL DE','Navarra') %>% #revisar para que salga solo Navarra
  str_replace_all('Navarra \\(Comun. Foral de\\)', 'Navarra') %>%
  str_replace_all('Navarra, Comunidad Foral de', 'Navarra') %>% 
  str_replace_all('PAÍS VASCO', 'País Vasco') %>% 
  str_replace_all("Palmas \\(Las\\)", 'Las Palmas') %>% #revisar el Palmas (Las) no se ha modificado con esto
  str_replace_all("Palmas, Las", 'Las Palmas') %>%
  str_replace_all("Rioja \\(La\\)", 'La Rioja') %>% #revisar Rioja (La) no se ha modificado con esto
  str_replace_all("Rioja, La", 'La Rioja') %>%
  str_replace_all("RIOJA, LA", 'La Rioja') %>% 
  str_replace_all("TOTAL NACIONAL", 'Total') %>% 
  str_replace_all("Total Nacional", 'Total') %>% 
  str_replace_all("/València", '') %>% 
  str_replace_all("Bizkaia", 'Vizcaya')

<<<<<<< HEAD
factor(df_i$Provincia.de.hospitalización)
length(levels(factor(df_i$Provincia.de.hospitalización)))
 
=======
typeof(df_i$Provincia.de.hospitalización)
  
#Cambia varones por hombres
df_i$Sexo <- str_replace_all(df_i$Sexo,"Varones", 'Hombres')


#Esto es para crear una nueva columna con la comunidad autonoma a la que corresponde cada provincia
#Necesario para tener una mejor vision de los diagramas de dispersion (puede que exista una forma mas eficiente)
mapeo_comunidades <- c(
  "A Coruña" = "Galicia",
  "Alava" = "Pais Vasco",
  "Albacete" = "Castilla - La Mancha",
  "Almería" = "Andalucía",
  "Alicante" = "Comunidad Valenciana"
  )
df_i <- df_i %>%
  mutate(Comunidad.Autonoma = mapeo_comunidades[Provincia.de.hospitalización])
  

>>>>>>> 824e55ea311675fe9e0aa09e98de640266521f4d
#Lo mismo para los datos de muertes
datos_muertes <- read.px('INPUT/DATA/Diabetes/Muertes/muertes1997-2022.px')
datos_muertes
df_m <- as.data.frame(datos_muertes)
head(df_m)


#--------------------------------------------------
# Aplicacion shiny (donde generaremos diversos datagramas y graficas)
#--------------------------------------------------
library(shiny)
library(ggplot2)
library(plotly)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Diagrama de dispersión de casos por año y provincia"),
  sidebarLayout(
    sidebarPanel(
      selectInput("sex", "Seleccione el sexo:", choices = NULL)           # Se llenará dinámicamente
    ),
    mainPanel(
      plotlyOutput("scatterPlot", width = "100%", height = "600px")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  # Actualiza las opciones de sexo basadas en los datos cargados
  observe({
    updateSelectInput(session, "sex", choices = unique(df_i$Sexo))
  })
  
  # Renderiza el gráfico de dispersión
  output$scatterPlot <- renderPlotly({
    #req(input$sexo)  # Asegura que se haya seleccionado un sexo
    
    # Filtra los datos por el sexo seleccionado
    datos_filtrados <- subset(df_i, Sexo == input$sex)
    
    # Crea el gráfico con ggplot2
    p<- ggplot(df_i, aes(x = Año, y = value, color = Provincia.de.hospitalización)) +
      geom_point() +
      labs(title = paste("Casos por año y provincia (Sexo:", input$sex, ")"),
           x = "Año", y = "Casos") +
      theme_minimal()
    
    ggplotly(p)
  })
}

# Correr la aplicación
#para que la aplicacion funcione descomentar la siguiente linea (comentada para que no se de por error y se os ejecute)
shinyApp(ui, server)




