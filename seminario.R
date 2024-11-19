

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

#Union de datos_met y cod_estaciones
datos_met <- merge(datos_met,cod_estaciones, by = 'codigo')

#Modificacion de datos y columnas para tener los mismos valores en todos los df
datos_met<-rename(.data = datos_met, Provincia = provincia, Año = año)
datos_met <- select(datos_met,-codigo)
datos_met$Año <- as.character(datos_met$Año)

datos_met$Provincia <- str_replace_all(datos_met$Provincia,"TARRAGONA","Tarragona") %>% 
  str_replace_all("BARCELONA", "Barcelona") %>% 
  str_replace_all("GIRONA", "Girona") %>% 
  str_replace_all("GIPUZKOA", "Gipuzkoa") %>% 
  str_replace_all("BIZKAIA", "Vizcaya") %>% 
  str_replace_all("CANTABRIA", 'Cantabria') %>% 
  str_replace_all('ASTURIAS','Asturias') %>% 
  str_replace_all("A CORUÑA", "A Coruña") %>% 
  str_replace_all("PONTEVEDRA", "Pontevedra") %>% 
  str_replace_all("LUGO", "Lugo") %>% 
  str_replace_all("OURENSE", "Ourense") %>% 
  str_replace_all("SORIA", "Soria") %>% 
  str_replace_all("PALENCIA", "Palencia") %>% 
  str_replace_all("BURGOS", "Burgos") %>% 
  str_replace_all("VALLADOLID", "Valladolid") %>% 
  str_replace_all("AVILA", "Avila") %>% 
  str_replace_all("SEGOVIA", "Segovia") %>% 
  str_replace_all("ZAMORA", "Zamora") %>% 
  str_replace_all("LEON", "León") %>%
  str_replace_all("SALAMANCA", "Salamanca") %>% 
  str_replace_all("GUADALAJARA", "Guadalajara") %>% 
  str_replace_all("MADRID", "Madrid") %>% 
  str_replace_all("TOLEDO", "Toledo") %>% 
  str_replace_all("CACERES", "Cáceres") %>% 
  str_replace_all("CIUDAD REAL", "Ciudad Real") %>% 
  str_replace_all("BADAJOZ", "Badajoz") %>% 
  str_replace_all("HUELVA", "Huelva") %>% 
  str_replace_all("CEUTA", "Ceuta") %>% 
  str_replace_all("JAEN", "Jaén") %>% 
  str_replace_all("CORDOBA", "Córdoba") %>% 
  str_replace_all("GRANADA", "Granada") %>% 
  str_replace_all("SEVILLA", "Sevilla") %>% 
  str_replace_all("CADIZ", "Cádiz") %>% 
  str_replace_all("MELILLA", "Melilla") %>% 
  str_replace_all("MALAGA", "Málaga") %>% 
  str_replace_all("ALMERIA", "Almería") %>% 
  str_replace_all("MURCIA", "Murcia") %>% 
  str_replace_all("ALICANTE", "Alicante") %>% 
  str_replace_all("CUENCA", "Cuenca") %>% 
  str_replace_all("ALBACETE", "Albacete") %>% 
  str_replace_all("TERUEL", "Teruel") %>% 
  str_replace_all("VALENCIA", "Valencia") %>% 
  str_replace_all("CASTELLON", "Castellón") %>% 
  str_replace_all("ARABA/ALAVA", "Alava") %>% 
  str_replace_all("LA RIOJA", "La Rioja") %>% 
  str_replace_all("NAVARRA", "Navarra") %>% 
  str_replace_all("ZARAGOZA", "Zaragoza") %>% 
  str_replace_all("LLEIDA", "Lleida") %>% 
  str_replace_all("HUESCA", "Huesca") %>% 
  str_replace_all("ILLES BALEARS", "Baleares") %>% 
  str_replace_all("STA. CRUZ DE TENERIFE", "Santa Cruz de Tenerife") %>% 
  str_replace_all("LAS PALMAS", "Las Palmas")

#Dataframe de datos_met lista para combianarse con otro dataframe



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
#----------------------------------------------------------------
#Modificacion de dataframe df_i
#----------------------------------------------------------------
# dataframe con todos los datos de ingresos de todos los años
df_i
df_i <- select(df_i,-Diagnóstico)

head(df_i)
length(levels(factor(df_i$Sexo)))

# análisis de los valores/niveles de cada atributo
length(levels(factor(df_i$Provincia.de.hospitalización)))

#Retira los numeros de la provincia y posteriormente modifica los Nombres de las provincias para tener unos standar iguales a todos
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
  str_replace_all('Madrid \\(Comunidad de\\)', 'Madrid') %>% 
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

#Cambia varones por hombres
df_i$Sexo <- str_replace_all(df_i$Sexo,"Varones", 'Hombres')

#Creacion de una nueva columna Comunidad.Autonoma segun la Provincia
df_i<- df_i %>% 
  mutate(Comunidad.Autonoma = case_when(
    Provincia.de.hospitalización %in% c("Almería","Cádiz","Córdoba","Granada","Huelva","Jaén","Málaga","Sevilla") ~ "Andalucía",
    Provincia.de.hospitalización %in% c("Huesca","Teruel","Zaragoza") ~ "Aragón",
    Provincia.de.hospitalización == "Baleares" ~ "Baleares",
    Provincia.de.hospitalización == "Asturias" ~ "Asturias",
    Provincia.de.hospitalización %in% c("Las Palmas","Santa Cruz de Tenerife") ~ "Canarias",
    Provincia.de.hospitalización == "Cantabria" ~ "Cantabria",
    Provincia.de.hospitalización %in% c("Avila","Burgos", "León", "Palencia", "Salamanca", "Segovia", "Soria", "Valladolid", "Zamora") ~ "Castilla y León",
    Provincia.de.hospitalización %in% c("Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo") ~ "Castilla - La Mancha",
    Provincia.de.hospitalización %in% c("Barcelona", "Girona", "Lleida", "Tarragona") ~ "Cataluña",
    Provincia.de.hospitalización %in% c("Alicante", "Castellón", "Valencia") ~ "Comunidad Valenciana",
    Provincia.de.hospitalización %in% c("Badajoz", "Cáceres") ~ "Extremadura",
    Provincia.de.hospitalización %in% c("A Coruña", "Lugo", "Ourense", "Pontevedra") ~ "Galicia", 
    Provincia.de.hospitalización == "Madrid" ~ "Madrid",
    Provincia.de.hospitalización == "Murcia" ~ "Murcia",
    Provincia.de.hospitalización == "Navarra" ~ "Navarra",
    Provincia.de.hospitalización %in% c("Alava", "Gipuzkoa", "Vizcaya") ~ "País Vasco",
    Provincia.de.hospitalización == "La Rioja" ~ "La Rioja",
    Provincia.de.hospitalización == "Ceuta" ~ "Ceuta",
    Provincia.de.hospitalización == "Melilla" ~ "Melilla"
  )) %>% 
  filter(!grepl("^(Total|Andalucía|Aragón|Canarias|Castilla y León|Castilla - La Mancha|Cataluña|Comunidad Valenciana|Extremadura|Galicia|País Vasco)"
                , Provincia.de.hospitalización)) #se retiran los valores globales de las comunidades autonomas de mas de 2 provincias

#Standarizacion de nombres en las columnas
df_i <- rename(.data = df_i, Provincia = Provincia.de.hospitalización, Diagnosticos = value)

#dataframe df_i listo para convinarse con otro dataframe


#----------------------------------------
# DATOS Muertes por diabetes
#----------------------------------------
datos_muertes <- read.px('INPUT/DATA/Diabetes/Muertes/muertes1997-2022.px')
datos_muertes
df_m <- as.data.frame(datos_muertes)
head(df_m)

#---------------------------------------------------------------------
#Modificacion de dataframe df_m
#---------------------------------------------------------------------
#Retira los numeros de la provincia y posteriormente modifica los Nombres de las provincias para tener unos standar iguales a todos
df_m$Provincias <- str_replace_all(df_m$Provincias,"[0123456789]","") %>% 
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
  str_replace_all('Madrid \\(Comunidad de\\)', 'Madrid') %>% 
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

#Creacion de una nueva columna Comunidad.Autonoma segun la Provincia
df_m <- df_m %>%
  mutate(Comunidad.Autonoma = case_when(
    Provincias %in% c("Almería","Cádiz","Córdoba","Granada","Huelva","Jaén","Málaga","Sevilla") ~ "Andalucía",
    Provincias %in% c("Huesca","Teruel","Zaragoza") ~ "Aragón",
    Provincias == "Baleares" ~ "Baleares",
    Provincias == "Asturias" ~ "Asturias",
    Provincias %in% c("Las Palmas","Santa Cruz de Tenerife") ~ "Canarias",
    Provincias == "Cantabria" ~ "Cantabria",
    Provincias %in% c("Avila","Burgos", "León", "Palencia", "Salamanca", "Segovia", "Soria", "Valladolid", "Zamora") ~ "Castilla y León",
    Provincias %in% c("Albacete", "Ciudad Real", "Cuenca", "Guadalajara", "Toledo") ~ "Castilla - La Mancha",
    Provincias %in% c("Barcelona", "Girona", "Lleida", "Tarragona") ~ "Cataluña",
    Provincias %in% c("Alicante", "Castellón", "Valencia") ~ "Comunidad Valenciana",
    Provincias %in% c("Badajoz", "Cáceres") ~ "Extremadura",
    Provincias %in% c("A Coruña", "Lugo", "Ourense", "Pontevedra") ~ "Galicia", 
    Provincias == "Madrid" ~ "Madrid",
    Provincias == "Murcia" ~ "Murcia",
    Provincias == "Navarra" ~ "Navarra",
    Provincias %in% c("Alava", "Gipuzkoa", "Vizcaya") ~ "País Vasco",
    Provincias == "La Rioja" ~ "La Rioja",
    Provincias == "Ceuta" ~ "Ceuta",
    Provincias == "Melilla" ~ "Melilla"
  )) %>% 
  filter(!grepl("^(Extranjero|Nacional|Total|Andalucía|Aragón|Canarias|Castilla y León|Castilla - La Mancha|Cataluña|Comunidad Valenciana|Extremadura|Galicia|País Vasco)"
                , Provincias))   #se retiran los valores globales de las comunidades autonomas de mas de 2 provincias

#Standarizacion y retirada de columnas que no aportan valor
df_m <- select(df_m,-Causa.de.muerte)
levels(factor(df_m$Sexo))
df_m$Sexo <-  str_replace_all(df_m$Sexo,"Total","Ambos sexos")
df_m<-rename(.data = df_m, Provincia = Provincias, Muertes = value, Año = Periodo)

#--------------------------------------------
#Union de todos los dataframe en el mismo (df_combined)
#--------------------------------------------

#Union de df_i y de df_m

df_combined <- left_join(x = df_i, y = df_m, by = c("Provincia", "Año", "Comunidad.Autonoma", "Sexo"))

#Union de df_combined y de datos_met
  
df_combined <- left_join(x = df_combined, y = datos_met, by = c("Provincia", "Año"))

#Guardar df_combined en un nuevo RData
save(df_combined,file = './Datos_Cargados.RData')


#Carga de df_combined

load(file = 'Datos_Cargados.RData')


#--------------------------------------------------
# Aplicacion shiny (donde generaremos diversos datagramas y graficas)
#--------------------------------------------------
library(shiny)
library(ggplot2)
library(plotly)

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Selecciona y visualiza gráficos"),
  sidebarLayout(
    sidebarPanel(
      selectInput("graph_choice", "Seleccione el gráfico a mostrar:",
                  choices = c("Casos por año y provincia" = "grafico1",
                              "Temperatura vs número de casos" = "grafico2")),
      
      # Controles para el primer gráfico
      conditionalPanel(
        condition = "input.graph_choice == 'grafico1'",
        div(
          style = "width: 150px;",
          selectInput("sex", "Seleccione el sexo:", choices = NULL),
          selectInput("valor", "Seleccione el Diagnosticos/muertes:", choices = c("Diagnosticos", "Muertes"))
        )
      ),
      
      # Controles para el segundo gráfico
      conditionalPanel(
        condition = "input.graph_choice == 'grafico2'",
        div(
          selectInput("temp_var", "Seleccione la variable de temperatura:",
                      choices = c("ta_max", "ta_min", "tm_mes")),
          selectInput("sex_temp", "Seleccione el sexo:", choices = NULL),
          selectInput("valor_temp", "Seleccione el Diagnosticos/muertes:", choices = c("Diagnosticos", "Muertes"))
          
        )
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.graph_choice == 'grafico1'",
        plotlyOutput("scatterPlot1", width = "130%", height = "700px")
      ),
      conditionalPanel(
        condition = "input.graph_choice == 'grafico2'",
        plotlyOutput("scatterPlot2", width = "130%", height = "700px")
      )
    )
  )
)

# Servidor
server <- function(input, output, session) {
  # Actualiza las opciones de sexo basadas en los datos cargados
  observe({
    updateSelectInput(session, "sex", choices = unique(df_combined$Sexo))
    updateSelectInput(session, "sex_temp", choices = unique(df_combined$Sexo))
  })
  
  # Renderiza el primer gráfico de dispersión
  output$scatterPlot1 <- renderPlotly({
    req(input$graph_choice == "grafico1")
    
    datos_filtrados <- subset(df_combined, Sexo == input$sex)
    
    p <- ggplot(datos_filtrados, aes_string(x = "Año", y = input$valor, color = "Comunidad.Autonoma",
                                     text = "paste('Provincia:', Provincia)")) +
      geom_point() +
      labs(title = paste(input$valor, "por año y provincia (Sexo:", input$sex, ")"),
           x = "Año", y = input$valor) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Renderiza el segundo gráfico de dispersión
  output$scatterPlot2 <- renderPlotly({
    req(input$graph_choice == "grafico2")
    
    datos_filtrados_temp <- subset(df_combined, Sexo == input$sex_temp)
    
    p2 <- ggplot(datos_filtrados_temp, aes_string(
      x = input$temp_var,
      y = input$valor_temp,
      color = "Comunidad.Autonoma",
      text = "paste('Provincia:', Provincia, '<br>Año:', Año)"
    )) +
      geom_point() +
      labs(title = paste("Relación entre", input$temp_var, "y",input$valor_temp, "(Sexo:", input$sex, ")"),
           x = input$temp_var, y = "Número de:", input$valor_temp) +
      theme_minimal()
    
    ggplotly(p2)
  })
}

# Correr la aplicación
# Descomentar la siguiente línea para ejecutar la aplicación en tu entorno local.
shinyApp(ui, server)

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

