library(shiny)
library(tidyverse)
library(lubridate)

# Cargar la base
# Cambiá el nombre del archivo y la función de importación si corresponde
df1 <- readRDS("df1.rds")

# Preparar los datos
df_app <- df1 %>%
  filter(Estadia > 0) %>%
  mutate(
    GastoDiario = GastoTotal / Estadia,
    Año = year(FechaIngreso),
    Trimestre = paste0("T", quarter(FechaIngreso))
  )

# Interfaz
ui <- fluidPage(
  
  titlePanel("Turismo receptivo en Uruguay"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        inputId = "anio",
        label = "Seleccione un año:",
        choices = sort(unique(df_app$Año)),
        selected = max(df_app$Año)
      )
    ),
    
    mainPanel(
      plotOutput("grafico_trimestre")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  
  datos_filtrados <- reactive({
    
    df_app %>%
      filter(Año == input$anio)
    
  })
  
  output$grafico_trimestre <- renderPlot({
    
    datos_filtrados() %>%
      group_by(Trimestre) %>%
      summarise(
        gasto_promedio_diario = mean(GastoDiario),
        .groups = "drop"
      ) %>%
      ggplot(
        aes(
          x = Trimestre,
          y = gasto_promedio_diario
        )
      ) +
      geom_col(fill = "steelblue") +
      labs(
        title = paste(
          "Gasto diario promedio por trimestre -",
          input$anio
        ),
        x = "Trimestre",
        y = "Gasto diario promedio"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
