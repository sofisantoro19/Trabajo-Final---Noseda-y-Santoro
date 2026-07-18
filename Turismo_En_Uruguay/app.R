library(shiny)
library(ggplot2)

# Cargar la base -------------------------------------------------------

df1 <- readRDS("../df1.rds")

# Preparación mínima de variables -------------------------------------

df1$FechaIngreso <- as.Date(df1$FechaIngreso)

# Crear año cuando todavía no exista
if (!"Anio" %in% names(df1)) {
  df1$Anio <- as.numeric(format(df1$FechaIngreso, "%Y"))
}

# Crear mes cuando todavía no exista
if (!"Mes" %in% names(df1)) {
  
  numero_mes <- as.numeric(
    format(df1$FechaIngreso, "%m")
  )
  
  df1$Mes <- factor(
    numero_mes,
    levels = 1:12,
    labels = c(
      "Enero",
      "Febrero",
      "Marzo",
      "Abril",
      "Mayo",
      "Junio",
      "Julio",
      "Agosto",
      "Setiembre",
      "Octubre",
      "Noviembre",
      "Diciembre"
    )
  )
}

# Crear gasto diario cuando todavía no exista
if (!"GastoDiario" %in% names(df1)) {
  df1$GastoDiario <- df1$GastoTotal / df1$Estadia
}

# Crear trimestre cuando todavía no exista
if (!"Trimestre" %in% names(df1)) {
  
  numero_mes <- as.numeric(
    format(df1$FechaIngreso, "%m")
  )
  
  df1$Trimestre <- paste0(
    "T",
    ceiling(numero_mes / 3)
  )
}

# Conservar el período del trabajo
df_app <- df1[
  df1$Anio >= 2016 &
    df1$Anio <= 2024,
]

# Variables de rubros de gasto
variables_rubros <- grep(
  "^Gasto",
  names(df_app),
  value = TRUE
)

variables_rubros <- setdiff(
  variables_rubros,
  c("GastoTotal", "GastoDiario")
)

# Función para mostrar porcentajes sin usar scales
etiqueta_porcentaje <- function(x) {
  paste0(
    round(x * 100),
    "%"
  )
}

# Interfaz -------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      HTML("
        body {
          background-color: #f4f6f8;
        }

        .titulo-app {
          background-color: steelblue;
          color: white;
          padding: 18px;
          border-radius: 5px;
          margin-bottom: 20px;
        }

        .panel-grafico {
          background-color: white;
          padding: 15px;
          margin-bottom: 20px;
          border-radius: 5px;
        }

        .indicador {
          background-color: white;
          border-left: 6px solid steelblue;
          padding: 15px;
          margin-bottom: 20px;
          border-radius: 5px;
        }

        .indicador-titulo {
          color: #555555;
          font-size: 15px;
        }

        .indicador-valor {
          color: #222222;
          font-size: 25px;
          margin-top: 5px;
        }
      ")
    )
  ),
  
  div(
    class = "titulo-app",
    h2("Turismo receptivo en Uruguay"),
    p("Análisis del período 2016-2024")
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Filtros"),
      
      selectInput(
        inputId = "filtro_anio",
        label = "Año:",
        choices = c(
          "Todos",
          sort(unique(df_app$Anio))
        ),
        selected = "Todos"
      ),
      
      selectInput(
        inputId = "filtro_pais",
        label = "País o región de residencia:",
        choices = c(
          "Todos",
          sort(
            unique(
              na.omit(df_app$Pais_residencia)
            )
          )
        ),
        selected = "Todos"
      ),
      
      selectInput(
        inputId = "filtro_motivo",
        label = "Motivo del viaje:",
        choices = c(
          "Todos",
          sort(
            unique(
              na.omit(df_app$Motivo)
            )
          )
        ),
        selected = "Todos"
      ),
      
      selectInput(
        inputId = "filtro_destino",
        label = "Destino:",
        choices = c(
          "Todos",
          sort(
            unique(
              na.omit(df_app$Destino)
            )
          )
        ),
        selected = "Todos"
      ),
      
      br(),
      
      actionButton(
        inputId = "reiniciar",
        label = "Reiniciar filtros"
      )
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        # PESTAÑA 1 ----------------------------------------------------
        
        tabPanel(
          title = "Origen y estacionalidad",
          
          fluidRow(
            
            column(
              width = 4,
              
              div(
                class = "indicador",
                
                div(
                  class = "indicador-titulo",
                  "Cantidad de visitantes"
                ),
                
                div(
                  class = "indicador-valor",
                  textOutput("total_visitantes")
                )
              )
            ),
            
            column(
              width = 4,
              
              div(
                class = "indicador",
                
                div(
                  class = "indicador-titulo",
                  "Principal país o región"
                ),
                
                div(
                  class = "indicador-valor",
                  textOutput("principal_pais")
                )
              )
            ),
            
            column(
              width = 4,
              
              div(
                class = "indicador",
                
                div(
                  class = "indicador-titulo",
                  "Principal continente"
                ),
                
                div(
                  class = "indicador-valor",
                  textOutput("principal_continente")
                )
              )
            )
          ),
          
          fluidRow(
            
            column(
              width = 6,
              
              div(
                class = "panel-grafico",
                
                plotOutput(
                  outputId = "grafico_paises",
                  height = "480px"
                )
              )
            ),
            
            column(
              width = 6,
              
              div(
                class = "panel-grafico",
                
                plotOutput(
                  outputId = "grafico_continentes",
                  height = "480px"
                )
              )
            )
          ),
          
          fluidRow(
            
            column(
              width = 6,
              
              div(
                class = "panel-grafico",
                
                plotOutput(
                  outputId = "grafico_meses",
                  height = "430px"
                )
              )
            ),
            
            column(
              width = 6,
              
              div(
                class = "panel-grafico",
                
                plotOutput(
                  outputId = "grafico_meses_anios",
                  height = "430px"
                )
              )
            )
          )
        ),
        
        # PESTAÑA 2 ----------------------------------------------------
        
        tabPanel(
          title = "Motivo y destino",
          
          div(
            class = "panel-grafico",
            
            plotOutput(
              outputId = "grafico_motivo",
              height = "450px"
            )
          ),
          
          div(
            class = "panel-grafico",
            
            plotOutput(
              outputId = "grafico_motivo_destino_porcentaje",
              height = "550px"
            )
          ),
          
          div(
            class = "panel-grafico",
            
            plotOutput(
              outputId = "grafico_motivo_destino_cantidad",
              height = "550px"
            )
          ),
          
          div(
            class = "panel-grafico",
            
            plotOutput(
              outputId = "grafico_destino_continente",
              height = "550px"
            )
          )
        ),
        
        # PESTAÑA 3 ----------------------------------------------------
        
        tabPanel(
          title = "Estadía",
          
          fluidRow(
            
            column(
              width = 6,
              
              div(
                class = "indicador",
                
                div(
                  class = "indicador-titulo",
                  "Estadía promedio"
                ),
                
                div(
                  class = "indicador-valor",
                  textOutput("estadia_promedio")
                )
              )
            ),
            
            column(
              width = 6,
              
              div(
                class = "indicador",
                
                div(
                  class = "indicador-titulo",
                  "Estadía mediana"
                ),
                
                div(
                  class = "indicador-valor",
                  textOutput("estadia_mediana")
                )
              )
            )
          ),
          
          fluidRow(
            
            column(
              width = 6,
              
              div(
                class = "panel-grafico",
                
                plotOutput(
                  outputId = "grafico_distribucion_estadia",
                  height = "430px"
                )
              )
            ),
            
            column(
              width = 6,
              
              div(
                class = "panel-grafico",
                
                plotOutput(
                  outputId = "grafico_caja_estadia",
                  height = "430px"
                )
              )
            )
          ),
          
          div(
            class = "panel-grafico",
            
            plotOutput(
              outputId = "grafico_estadia_destino",
              height = "520px"
            )
          ),
          
          div(
            class = "panel-grafico",
            
            plotOutput(
              outputId = "grafico_estadia_motivo",
              height = "520px"
            )
          )
        ),
        
        # PESTAÑA 4 ----------------------------------------------------
        
        tabPanel(
          title = "Gasto",
          
          fluidRow(
            
            column(
              width = 6,
              
              div(
                class = "indicador",
                
                div(
                  class = "indicador-titulo",
                  "Gasto diario promedio"
                ),
                
                div(
                  class = "indicador-valor",
                  textOutput("gasto_promedio")
                )
              )
            ),
            
            column(
              width = 6,
              
              div(
                class = "indicador",
                
                div(
                  class = "indicador-titulo",
                  "Gasto diario mediano"
                ),
                
                div(
                  class = "indicador-valor",
                  textOutput("gasto_mediano")
                )
              )
            )
          ),
          
          div(
            class = "panel-grafico",
            
            plotOutput(
              outputId = "grafico_rubros",
              height = "500px"
            )
          ),
          
          fluidRow(
            
            column(
              width = 6,
              
              div(
                class = "panel-grafico",
                
                plotOutput(
                  outputId = "grafico_gasto_destino",
                  height = "480px"
                )
              )
            ),
            
            column(
              width = 6,
              
              div(
                class = "panel-grafico",
                
                plotOutput(
                  outputId = "grafico_gasto_motivo",
                  height = "480px"
                )
              )
            )
          )
        )
      )
    )
  )
)

# Servidor -------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reiniciar filtros
  observeEvent(input$reiniciar, {
    
    updateSelectInput(
      session,
      "filtro_anio",
      selected = "Todos"
    )
    
    updateSelectInput(
      session,
      "filtro_pais",
      selected = "Todos"
    )
    
    updateSelectInput(
      session,
      "filtro_motivo",
      selected = "Todos"
    )
    
    updateSelectInput(
      session,
      "filtro_destino",
      selected = "Todos"
    )
  })
  
  # Aplicación de los filtros
  datos_filtrados <- reactive({
    
    datos <- df_app
    
    if (input$filtro_anio != "Todos") {
      
      datos <- datos[
        datos$Anio ==
          as.numeric(input$filtro_anio),
      ]
    }
    
    if (input$filtro_pais != "Todos") {
      
      datos <- datos[
        datos$Pais_residencia ==
          input$filtro_pais,
      ]
    }
    
    if (input$filtro_motivo != "Todos") {
      
      datos <- datos[
        datos$Motivo ==
          input$filtro_motivo,
      ]
    }
    
    if (input$filtro_destino != "Todos") {
      
      datos <- datos[
        datos$Destino ==
          input$filtro_destino,
      ]
    }
    
    datos
  })
  
  # Indicadores --------------------------------------------------------
  
  output$total_visitantes <- renderText({
    
    format(
      nrow(datos_filtrados()),
      big.mark = "."
    )
  })
  
  output$principal_pais <- renderText({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Pais_residencia),
    ]
    
    if (nrow(datos) == 0) {
      return("Sin datos")
    }
    
    tabla <- sort(
      table(datos$Pais_residencia),
      decreasing = TRUE
    )
    
    names(tabla)[1]
  })
  
  output$principal_continente <- renderText({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Continente),
    ]
    
    if (nrow(datos) == 0) {
      return("Sin datos")
    }
    
    tabla <- sort(
      table(datos$Continente),
      decreasing = TRUE
    )
    
    names(tabla)[1]
  })
  
  output$estadia_promedio <- renderText({
    
    datos <- datos_filtrados()
    
    paste0(
      round(
        mean(
          datos$Estadia,
          na.rm = TRUE
        ),
        1
      ),
      " días"
    )
  })
  
  output$estadia_mediana <- renderText({
    
    datos <- datos_filtrados()
    
    paste0(
      round(
        median(
          datos$Estadia,
          na.rm = TRUE
        ),
        1
      ),
      " días"
    )
  })
  
  output$gasto_promedio <- renderText({
    
    datos <- datos_filtrados()
    
    paste0(
      "USD ",
      format(
        round(
          mean(
            datos$GastoDiario,
            na.rm = TRUE
          ),
          1
        ),
        big.mark = ".",
        decimal.mark = ","
      )
    )
  })
  
  output$gasto_mediano <- renderText({
    
    datos <- datos_filtrados()
    
    paste0(
      "USD ",
      format(
        round(
          median(
            datos$GastoDiario,
            na.rm = TRUE
          ),
          1
        ),
        big.mark = ".",
        decimal.mark = ","
      )
    )
  })
  
  # Países o regiones --------------------------------------------------
  
  output$grafico_paises <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Pais_residencia),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- as.data.frame(
      table(datos$Pais_residencia)
    )
    
    names(tabla) <- c(
      "Pais_residencia",
      "n"
    )
    
    tabla <- tabla[
      tabla$n > 0,
    ]
    
    tabla$porcentaje <-
      tabla$n / sum(tabla$n) * 100
    
    ggplot(
      tabla,
      aes(
        x = reorder(
          Pais_residencia,
          porcentaje
        ),
        y = porcentaje
      )
    ) +
      geom_col(
        fill = "steelblue"
      ) +
      coord_flip() +
      labs(
        title = paste(
          "Principales países o regiones",
          "de residencia de los visitantes"
        ),
        x = "País o región",
        y = "Porcentaje de visitantes"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom"
      )
  })
  
  # Continentes --------------------------------------------------------
  
  output$grafico_continentes <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Continente),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- as.data.frame(
      table(datos$Continente)
    )
    
    names(tabla) <- c(
      "Continente",
      "n"
    )
    
    tabla <- tabla[
      tabla$n > 0,
    ]
    
    tabla$porcentaje <-
      tabla$n / sum(tabla$n) * 100
    
    ggplot(
      tabla,
      aes(
        x = reorder(
          Continente,
          porcentaje
        ),
        y = porcentaje
      )
    ) +
      geom_col(
        fill = "steelblue"
      ) +
      coord_flip() +
      labs(
        title = "Proporción de visitantes por continente",
        x = "Continente",
        y = "Porcentaje de visitantes"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom"
      )
  })
  
  # Visitantes por mes -------------------------------------------------
  
  output$grafico_meses <- renderPlot({
    
    datos <- datos_filtrados()
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- as.data.frame(
      table(datos$Mes)
    )
    
    names(tabla) <- c(
      "Mes",
      "n"
    )
    
    tabla$porcentaje <-
      tabla$n / sum(tabla$n) * 100
    
    ggplot(
      tabla,
      aes(
        x = Mes,
        y = porcentaje
      )
    ) +
      geom_col(
        fill = "steelblue"
      ) +
      labs(
        title = paste(
          "Cantidad promedio de visitantes que ingresaron",
          "al país por mes durante 2016-2024"
        ),
        x = "Mes",
        y = "Porcentaje de visitantes"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom",
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  # Visitantes mensuales por año --------------------------------------
  
  output$grafico_meses_anios <- renderPlot({
    
    datos <- datos_filtrados()
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- as.data.frame(
      table(
        datos$Anio,
        datos$Mes
      )
    )
    
    names(tabla) <- c(
      "Anio",
      "Mes",
      "n"
    )
    
    tabla <- tabla[
      tabla$n > 0,
    ]
    
    tabla$porcentaje <-
      tabla$n / sum(tabla$n) * 100
    
    ggplot(
      tabla,
      aes(
        x = Mes,
        y = porcentaje,
        color = factor(Anio),
        group = Anio
      )
    ) +
      geom_line() +
      geom_point() +
      labs(
        title = paste(
          "Proporción de visitantes que ingresaron",
          "al país por mes durante 2016-2024"
        ),
        x = "Mes",
        y = "Porcentaje de visitantes",
        color = "Año"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom",
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        )
      )
  })
  
  # Motivo del viaje ---------------------------------------------------
  
  output$grafico_motivo <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Motivo),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- as.data.frame(
      table(datos$Motivo)
    )
    
    names(tabla) <- c(
      "Motivo",
      "n"
    )
    
    tabla <- tabla[
      tabla$n > 0,
    ]
    
    tabla$porcentaje <-
      tabla$n / sum(tabla$n) * 100
    
    ggplot(
      tabla,
      aes(
        x = reorder(
          Motivo,
          porcentaje
        ),
        y = porcentaje
      )
    ) +
      geom_col(
        fill = "steelblue"
      ) +
      coord_flip() +
      labs(
        title = paste(
          "Motivo de viaje de los visitantes",
          "durante 2016-2024"
        ),
        x = "Motivo del viaje",
        y = "Porcentaje de visitantes"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom"
      )
  })
  
  # Motivo según destino: porcentajes ---------------------------------
  
  output$grafico_motivo_destino_porcentaje <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Destino) &
        !is.na(datos$Motivo),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- prop.table(
      table(
        datos$Destino,
        datos$Motivo
      ),
      margin = 1
    )
    
    grafico <- as.data.frame(tabla)
    
    names(grafico) <- c(
      "Destino",
      "Motivo",
      "porcentaje"
    )
    
    grafico <- grafico[
      grafico$porcentaje > 0,
    ]
    
    ggplot(
      grafico,
      aes(
        x = Destino,
        y = porcentaje,
        fill = Motivo
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(
        labels = etiqueta_porcentaje
      ) +
      scale_fill_brewer(
        palette = "Set3"
      ) +
      labs(
        title = "Motivos del viaje según el destino elegido",
        subtitle = paste(
          "Distribución porcentual de los motivos",
          "dentro de cada destino, 2016-2024"
        ),
        x = "Destino",
        y = "Porcentaje de visitantes",
        fill = "Motivo del viaje"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom"
      )
  })
  
  # Motivo según destino: cantidades ---------------------------------
  
  output$grafico_motivo_destino_cantidad <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Destino) &
        !is.na(datos$Motivo),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- as.data.frame(
      table(
        datos$Destino,
        datos$Motivo
      )
    )
    
    names(tabla) <- c(
      "Destino",
      "Motivo",
      "n"
    )
    
    tabla <- tabla[
      tabla$n > 0,
    ]
    
    ggplot(
      tabla,
      aes(
        x = reorder(
          Destino,
          n,
          FUN = sum
        ),
        y = n,
        fill = Motivo
      )
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste(
          "Cantidad de visitantes según destino",
          "y motivo del viaje"
        ),
        subtitle = "Turismo receptivo en Uruguay, 2016-2024",
        x = "Destino",
        y = "Cantidad de observaciones",
        fill = "Motivo del viaje"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom"
      )
  })
  
  # Destino según continente ------------------------------------------
  
  output$grafico_destino_continente <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Continente) &
        !is.na(datos$Destino),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- prop.table(
      table(
        datos$Continente,
        datos$Destino
      ),
      margin = 1
    )
    
    grafico <- as.data.frame(tabla)
    
    names(grafico) <- c(
      "Continente",
      "Destino",
      "porcentaje"
    )
    
    grafico <- grafico[
      grafico$porcentaje > 0,
    ]
    
    ggplot(
      grafico,
      aes(
        x = Continente,
        y = porcentaje,
        fill = Destino
      )
    ) +
      geom_col() +
      coord_flip() +
      scale_y_continuous(
        labels = etiqueta_porcentaje
      ) +
      labs(
        title = "Destino elegido según la región de residencia",
        subtitle = paste(
          "Distribución porcentual de los destinos",
          "dentro de cada región, 2016-2024"
        ),
        x = "Región de residencia",
        y = "Porcentaje de visitantes",
        fill = "Destino"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom"
      )
  })
  
  # Distribución de la estadía ----------------------------------------
  
  output$grafico_distribucion_estadia <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Estadia),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    tabla <- as.data.frame(
      table(datos$Estadia)
    )
    
    names(tabla) <- c(
      "Estadia",
      "n"
    )
    
    tabla$Estadia <- as.numeric(
      as.character(tabla$Estadia)
    )
    
    tabla <- tabla[
      tabla$Estadia <= 20,
    ]
    
    ggplot(
      tabla,
      aes(
        x = Estadia,
        y = n
      )
    ) +
      geom_col(
        fill = "steelblue"
      ) +
      scale_x_continuous(
        breaks = 0:20
      ) +
      labs(
        title = "Distribución de la duración de la estadía",
        x = "Días de estadía",
        y = "Cantidad de visitantes"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom"
      )
  })
  
  # Caja general de estadía -------------------------------------------
  
  output$grafico_caja_estadia <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Estadia),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    ggplot(
      datos,
      aes(y = Estadia)
    ) +
      geom_boxplot() +
      coord_cartesian(
        ylim = c(0, 20)
      ) +
      labs(
        title = paste(
          "Diagrama de caja de la duración",
          "de la estadía"
        ),
        x = NULL,
        y = "Días de estadía"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        legend.position = "bottom"
      )
  })
  
  # Estadía según destino ---------------------------------------------
  
  output$grafico_estadia_destino <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Destino) &
        !is.na(datos$Estadia),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    ggplot(
      datos,
      aes(
        x = Destino,
        y = Estadia,
        fill = Destino
      )
    ) +
      geom_boxplot() +
      coord_cartesian(
        ylim = c(0, 20)
      ) +
      labs(
        title = paste(
          "Diagrama de caja de la duración",
          "de la estadía según el destino"
        ),
        x = "Destino",
        y = "Días de estadía"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        legend.position = "none"
      )
  })
  
  # Estadía según motivo ----------------------------------------------
  
  output$grafico_estadia_motivo <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Motivo) &
        !is.na(datos$Estadia),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    ggplot(
      datos,
      aes(
        x = Motivo,
        y = Estadia,
        fill = Motivo
      )
    ) +
      geom_boxplot() +
      coord_cartesian(
        ylim = c(0, 20)
      ) +
      labs(
        title = paste(
          "Diagrama de caja de la duración",
          "de la estadía según el motivo"
        ),
        x = "Motivo del viaje",
        y = "Días de estadía"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(
          face = "bold",
          size = 12
        ),
        axis.text.x = element_text(
          angle = 45,
          hjust = 1
        ),
        legend.position = "none"
      )
  })
  
  # Participación de rubros -------------------------------------------
  
  output$grafico_rubros <- renderPlot({
    
    datos <- datos_filtrados()
    
    validate(
      need(
        length(variables_rubros) > 0,
        "No se encontraron variables de rubros de gasto"
      )
    )
    
    totales <- sapply(
      datos[
        ,
        variables_rubros,
        drop = FALSE
      ],
      sum,
      na.rm = TRUE
    )
    
    resumen_rubros <- data.frame(
      Rubro = names(totales),
      Gasto = as.numeric(totales)
    )
    
    resumen_rubros$Porcentaje <-
      resumen_rubros$Gasto /
      sum(resumen_rubros$Gasto) *
      100
    
    resumen_rubros$Rubro <- sub(
      "^Gasto",
      "",
      resumen_rubros$Rubro
    )
    
    ggplot(
      resumen_rubros,
      aes(
        x = reorder(
          Rubro,
          Porcentaje
        ),
        y = Porcentaje
      )
    ) +
      geom_col(
        fill = "steelblue"
      ) +
      coord_flip() +
      scale_y_continuous(
        labels = function(x) {
          paste0(
            round(x, 1),
            "%"
          )
        }
      ) +
      labs(
        title = paste(
          "Participación de cada rubro",
          "en el gasto turístico"
        ),
        subtitle = paste(
          "Porcentaje sobre el gasto total",
          "desagregado, 2016-2024"
        ),
        x = "Rubro",
        y = "Participación en el gasto total"
      ) +
      theme_minimal()
  })
  
  # Gasto según destino -----------------------------------------------
  
  output$grafico_gasto_destino <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Destino) &
        !is.na(datos$GastoDiario),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    gasto_por_destino <- aggregate(
      GastoDiario ~ Destino,
      data = datos,
      FUN = mean
    )
    
    names(gasto_por_destino)[
      names(gasto_por_destino) == "GastoDiario"
    ] <- "gasto_promedio_diario"
    
    ggplot(
      gasto_por_destino,
      aes(
        x = reorder(
          Destino,
          gasto_promedio_diario
        ),
        y = gasto_promedio_diario
      )
    ) +
      geom_col(
        fill = "steelblue"
      ) +
      coord_flip() +
      labs(
        title = "Gasto diario promedio según el destino",
        subtitle = "Turismo receptivo en Uruguay, 2016-2024",
        x = "Destino",
        y = "Gasto diario promedio"
      ) +
      theme_minimal()
  })
  
  # Gasto según motivo -------------------------------------------------
  
  output$grafico_gasto_motivo <- renderPlot({
    
    datos <- datos_filtrados()
    
    datos <- datos[
      !is.na(datos$Motivo) &
        !is.na(datos$GastoDiario),
    ]
    
    validate(
      need(
        nrow(datos) > 0,
        "No hay datos para los filtros seleccionados"
      )
    )
    
    gasto_por_motivo <- aggregate(
      GastoDiario ~ Motivo,
      data = datos,
      FUN = mean
    )
    
    names(gasto_por_motivo)[
      names(gasto_por_motivo) == "GastoDiario"
    ] <- "gasto_promedio_diario"
    
    ggplot(
      gasto_por_motivo,
      aes(
        x = reorder(
          Motivo,
          gasto_promedio_diario
        ),
        y = gasto_promedio_diario
      )
    ) +
      geom_col(
        fill = "steelblue"
      ) +
      coord_flip() +
      labs(
        title = paste(
          "Gasto diario promedio según",
          "el motivo del viaje"
        ),
        subtitle = "Turismo receptivo en Uruguay, 2016-2024",
        x = "Motivo del viaje",
        y = "Gasto diario promedio"
      ) +
      theme_minimal()
  })
}

shinyApp(
  ui = ui,
  server = server
)