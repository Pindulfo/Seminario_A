
library(pxR)
library(climaemet)
library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(mapSpain)
library(countrycode)
library(sf)





#Para ejecutar directamente el Shiny con todas las opciones 
load(file = 'Datos_Cargados_Completo.RData')

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
                  choices = c("Casos/Muertes (100.000 hab) por año y provincia" = "grafico1",
                              "Temperatura vs número de casos/Muertes (100.000 hab)" = "grafico2",
                              "Casos/Muertes (100.000 hab) de una provincia en Concreto (En años y en temperaturas)" = "grafico3",
                              "Mapa de España Casos (100.000 hab) en un año" = "grafico4"
                  )),
      
      # Controles para el primer gráfico
      conditionalPanel(
        condition = "input.graph_choice == 'grafico1'",
        div(
          style = "width: 150px;",
          selectInput("sex", "Seleccione el sexo:", choices = NULL),
          selectInput("valor", "Seleccione el Diagnosticos/muertes:", choices = c("Diagnosticos", "Muertes")),
          checkboxInput("filtro_año", "Mostrar datos desde 2010", value = FALSE),
          checkboxInput("Limites", "Mostrar intervalo de confianza", value = FALSE)
        )
      ),
      
      # Controles para el segundo gráfico
      conditionalPanel(
        condition = "input.graph_choice == 'grafico2'",
        div(
          selectInput("temp_var", "Seleccione la variable de temperatura:",
                      choices = c("ta_max", "ta_min", "tm_mes")),
          selectInput("sex_temp", "Seleccione el sexo:", choices = NULL),
          selectInput("valor_temp", "Seleccione el Diagnosticos/muertes:", choices = c("Diagnosticos", "Muertes")),
          checkboxInput("filtro_año_temp", "Mostrar datos desde 2010", value = FALSE),
          checkboxInput("Limites_temp", "Mostrar intervalo de confianza", value = FALSE)
          
        )
      ),
      #Controles del tercer grafico
      conditionalPanel(
        condition = "input.graph_choice == 'grafico3'",
        div(
          style = "width: 150px;",
          selectInput("provincia_1", "Seleccione la provincia:", choices = NULL),
          selectInput("selecion_1", "Seleccione la variable del eje de las X (Años, o temperaturas):",
                      choices = c("Año","ta_max", "ta_min", "tm_mes")),
          selectInput("sex_1", "Seleccione el sexo:", choices = NULL),
          selectInput("valor_1", "Seleccione el Diagnosticos/muertes:", choices = c("Diagnosticos", "Muertes")),
          checkboxInput("filtro_año_1", "Mostrar datos desde 2010", value = FALSE),
          checkboxInput("Limites_1", "Mostrar intervalo de confianza", value = FALSE)
          
        )
      ),
      #Controles del mapa de España
      conditionalPanel(
        condition = "input.graph_choice == 'grafico4'",
        div(
          selectInput("sex_map", "Seleccione el sexo:", choices = NULL),
          selectInput("año_map", "Selecione el año a mostrar", choices = NULL)
          
          
        )
      ),
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.graph_choice == 'grafico1'",
        plotlyOutput("scatterPlot1", width = "100%", height = "700px")
      ),
      conditionalPanel(
        condition = "input.graph_choice == 'grafico2'",
        plotlyOutput("scatterPlot2", width = "100%", height = "700px")
      ),
      conditionalPanel(
        condition = "input.graph_choice == 'grafico3'",
        plotlyOutput("scatterPlot3", width = "100%", height = "700px")
      ),
      conditionalPanel(
        condition = "input.graph_choice == 'grafico4'",
        plotlyOutput("scatterPlot4", width = "100%", height = "700px")
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
    updateSelectInput(session, "sex_1", choices = unique(df_combined$Sexo))
    updateSelectInput(session, "provincia_1", choices = unique(df_combined$Provincia))
    updateSelectInput(session, "sex_map", choices = unique(provincias_sf$Sexo))
    updateSelectInput(session, "año_map", choices = unique(provincias_sf$Año))
  })
  
  # Renderiza el primer gráfico de dispersión
  output$scatterPlot1 <- renderPlotly({
    req(input$graph_choice == "grafico1")
    
    datos_filtrados <- df_combined %>%
      filter(Sexo == input$sex)
    
    if (input$filtro_año) {
      datos_filtrados <- datos_filtrados %>% 
        filter(Año >= 2010)
    } 
    
    
    p <- ggplot(datos_filtrados, aes_string(x = "Año", y = input$valor, color = "Comunidad.Autonoma",
                                            group = "Comunidad.Autonoma", text = "paste('Provincia:', Provincia)"
    )) +
      geom_point() +
      geom_smooth(method = "lm", aes(color = Comunidad.Autonoma), se = input$Limites ) +
      labs(title = paste(input$valor, "(cada 100.000 hab) por año y provincia (Sexo:", input$sex, ")"),
           x = "Año", y = input$valor) +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Renderiza el segundo gráfico de dispersión
  output$scatterPlot2 <- renderPlotly({
    req(input$graph_choice == "grafico2")
    
    datos_filtrados_temp <- df_combined %>%
      filter(Sexo == input$sex_temp)
    
    if (input$filtro_año_temp) {
      datos_filtrados_temp <- datos_filtrados_temp %>% 
        filter(Año >= 2010)
    }
    
    p2 <- ggplot(datos_filtrados_temp, aes_string(
      x = input$temp_var,
      y = input$valor_temp,
      color = "Comunidad.Autonoma",
      text = "paste('Provincia:', Provincia, '<br>Año:', Año)"
    )) +
      geom_point(alpha = 0.6) +
      geom_smooth(
        method = "lm", 
        se = input$Limites_temp, 
        aes_string(x = input$temp_var, y = input$valor_temp), 
        inherit.aes = FALSE, 
        color = "black"
      ) +
      labs(title = paste("Relación entre", input$temp_var, "y",input$valor_temp, "(cada 100.000 hab) (Sexo:", input$sex, ")"),
           x = input$temp_var, y = input$valor_temp) +
      theme_minimal()
    
    ggplotly(p2)
  })
  output$scatterPlot3 <- renderPlotly({
    req(input$graph_choice == "grafico3")
    
    datos_filtrados_prov <- df_combined %>%
      filter(Sexo == input$sex_1 & Provincia == input$provincia_1)
    
    if (input$filtro_año_1) {
      datos_filtrados_prov<- datos_filtrados_prov %>% 
        filter(Año >= 2010)
    }
    
    p3 <- ggplot(datos_filtrados_prov, aes_string(x = input$selecion_1, 
                                                  y = input$valor_1, 
                                                  color = "Comunidad.Autonoma",
                                                  text = "paste('Provincia:', Provincia)"
    )) +
      geom_point()+
      geom_smooth(method = "lm", aes(color = input$provincia_1), se = input$Limites_1 )+
      labs(title = paste(input$valor_1, "(cada 100.000 hab) por", input$selecion_1, "y provincia",input$provincia_1, "(Sexo:", input$sex_1, ")"),
           x = input$selecion_1, y = input$valor_1) +
      theme_minimal()
    
    ggplotly(p3)
  })
  output$scatterPlot4 <- renderPlotly({
    req(input$graph_choice == "grafico4")
    
    datos_filtrados_map <- provincias_sf %>% 
      filter(Año == input$año_map & !is.na(ta_max) & Sexo == input$sex_map)
    
    interactive_map <- ggplot(datos_filtrados_map) +
      geom_sf(aes(fill = Diagnosticos, text = paste0(
        "Provincia: ", Provincia, "<br>",
        "Diagnósticos: ", Diagnosticos, "<br>",
        "Temp. Media: ", round(tm_mes, 1)
      )), color = "grey70", linewidth = 0.3) +  # Incluye información adicional en el hover
      geom_sf(data = can, color = "grey70") +  # Ajustar Canarias
      scale_fill_gradientn(
        colors = hcl.colors(10, "Reds", rev = TRUE),  # Escala de colores para diagnósticos
        n.breaks = 10,
        guide = guide_colorbar(title = "Diagnósticos")
      ) +
      labs(title = "Diagnósticos por provincia y temperatura media",
           fill = "Diagnósticos") +
      theme_void() +
      theme(legend.position = "right")
    
    # Usa plotly para interactividad
    ggplotly(interactive_map, tooltip = "text")
    
  })
}

# Correr la aplicación
# Descomentar la siguiente línea para ejecutar la aplicación en tu entorno local.
shinyApp(ui, server)
