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

# 4. Muertes por Diabetes por Comunidad Autónoma
df_m_filtrados <- df_m %>%
  group_by(Comunidad.Autonoma) %>%
  summarise(Muertes_Totales = sum(Muertes, na.rm = TRUE))

grafico_muertes <- ggplot(df_m_filtrados, aes(x = reorder(Comunidad.Autonoma, -Muertes_Totales), y = Muertes_Totales)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(
    title = "Muertes por Diabetes por Comunidad Autónoma",
    x = "Comunidad Autónoma",
    y = "Muertes Totales"
  ) +
  theme_minimal()

print(grafico_muertes) # Mostrar gráfico con ggplot

# Gráfico adicional: Relación entre Temperatura Media y Ingresos por Diabetes
# Preparamos los datos combinando los datasets
datos_combinados <- datos_met_filtrados %>%
  group_by(año) %>%
  summarise(Temperatura_Media = mean(tm_mes, na.rm = TRUE)) %>%
  inner_join(
    df_i %>%
      group_by(Año) %>%
      summarise(Ingresos_Totales = sum(Ingresos, na.rm = TRUE)),
    by = c("año" = "Año")
  )

# Creamos el gráfico
grafico_relacion <- ggplot(datos_combinados, aes(x = Temperatura_Media, y = Ingresos_Totales)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Relación entre Temperatura Media y Ingresos por Diabetes",
    x = "Temperatura Media (°C)",
    y = "Ingresos Totales por Diabetes"
  ) +
  theme_minimal()

print(grafico_relacion) # Mostrar gráfico con ggplot

