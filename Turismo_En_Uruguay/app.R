library(shiny)
library(tidyverse)
library(lubridate)
install.packages("bslib")
library(bslib)

df1 <- readRDS("../df1.rds")

library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(scales)

# Cargar el dataset procesado
df1 <- readRDS("../df1.rds")

# Preparar las variables que utilizará la aplicación
df_app <- df1 %>%
  filter(Estadia > 0) %>%
  mutate(
    GastoDiario = GastoTotal / Estadia,
    Anio = year(FechaIngreso),
    Trimestre = paste0("T", quarter(FechaIngreso))
  ) %>%
  filter(Anio >= 2016)

# Detectar automáticamente las variables correspondientes a rubros de gasto
variables_rubros <- names(df_app)[
  str_detect(names(df_app), regex("^Gasto", ignore_case = TRUE))
]

variables_rubros <- setdiff(
  variables_rubros,
  c("GastoTotal", "GastoDiario")
)

# INTERFAZ ---------------------------------------------------------------

ui <- page_sidebar(
  
  title = "Turismo receptivo en Uruguay",
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#4682B4"
  ),
  
  sidebar = sidebar(
    
    h4("Filtros"),
    
    selectInput(
      inputId = "anio",
      label = "Año:",
      choices = c(
        "Todos",
        sort(unique(df_app$Anio))
      ),
      selected = "Todos"
    ),
    
    selectInput(
      inputId = "trimestre",
      label = "Trimestre:",
      choices = c(
        "Todos",
        "T1",
        "T2",
        "T3",
        "T4"
      ),
      selected = "Todos"
    ),
    
    selectInput(
      inputId = "destino",
      label = "Destino:",
      choices = c(
        "Todos",
        sort(unique(df_app$Destino))
      ),
      selected = "Todos"
    ),
    
    selectInput(
      inputId = "motivo",
      label = "Motivo del viaje:",
      choices = c(
        "Todos",
        sort(unique(df_app$Motivo))
      ),
      selected = "Todos"
    ),
    
    hr(),
    
    actionButton(
      inputId = "reiniciar",
      label = "Reiniciar filtros",
      class = "btn-primary"
    )
  ),
  
  navset_card_tab(
    
    # PESTAÑA 1 ----------------------------------------------------------
    
    nav_panel(
      title = "Resumen general",
      
      layout_columns(
        
        value_box(
          title = "Observaciones",
          value = textOutput("total_visitantes"),
          showcase = icon("users"),
          theme = "primary"
        ),
        
        value_box(
          title = "Estadía promedio",
          value = textOutput("estadia_promedio"),
          showcase = icon("calendar"),
          theme = "info"
        ),
        
        value_box(
          title = "Gasto diario promedio",
          value = textOutput("gasto_promedio"),
          showcase = icon("dollar-sign"),
          theme = "success"
        ),
        
        value_box(
          title = "Principal destino",
          value = textOutput("principal_destino"),
          showcase = icon("location-dot"),
          theme = "warning"
        )
      ),
      
      layout_columns(
        
        card(
          full_screen = TRUE,
          card_header("Evolución anual del gasto diario"),
          plotOutput(
            outputId = "grafico_anual",
            height = "380px"
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("Gasto diario promedio por trimestre"),
          plotOutput(
            outputId = "grafico_trimestre",
            height = "380px"
          )
        )
      )
    ),
    
    # PESTAÑA 2 ----------------------------------------------------------
    
    nav_panel(
      title = "Perfil del viaje",
      
      layout_columns(
        
        card(
          full_screen = TRUE,
          card_header("Principales destinos"),
          plotOutput(
            outputId = "grafico_destinos",
            height = "450px"
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("Motivos del viaje"),
          plotOutput(
            outputId = "grafico_motivos",
            height = "450px"
          )
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Motivo del viaje según destino"),
        plotOutput(
          outputId = "grafico_destino_motivo",
          height = "500px"
        )
      )
    ),
    
    # PESTAÑA 3 ----------------------------------------------------------
    
    nav_panel(
      title = "Gasto turístico",
      
      layout_columns(
        
        card(
          full_screen = TRUE,
          card_header("Gasto diario promedio según destino"),
          plotOutput(
            outputId = "gasto_destino",
            height = "450px"
          )
        ),
        
        card(
          full_screen = TRUE,
          card_header("Gasto diario promedio según motivo"),
          plotOutput(
            outputId = "gasto_motivo",
            height = "450px"
          )
        )
      ),
      
      card(
        full_screen = TRUE,
        card_header("Participación de los rubros en el gasto"),
        plotOutput(
          outputId = "grafico_rubros",
          height = "500px"
        )
      )
    ),
    
    # PESTAÑA 4 ----------------------------------------------------------
    
    nav_panel(
      title = "Datos",
      
      card(
        card_header("Resumen de los datos filtrados"),
        tableOutput("tabla_resumen")
      )
    )
  )
)

# SERVIDOR ---------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reiniciar filtros
  observeEvent(input$reiniciar, {
    
    updateSelectInput(
      session,
      "anio",
      selected = "Todos"
    )
    
    updateSelectInput(
      session,
      "trimestre",
      selected = "Todos"
    )
    
    updateSelectInput(
      session,
      "destino",
      selected = "Todos"
    )
    
    updateSelectInput(
      session,
      "motivo",
      selected = "Todos"
    )
  })
  
  # Dataset reactivo
  datos_filtrados <- reactive({
    
    datos <- df_app
    
    if (input$anio != "Todos") {
      datos <- datos %>%
        filter(Anio == as.numeric(input$anio))
    }
    
    if (input$trimestre != "Todos") {
      datos <- datos %>%
        filter(Trimestre == input$trimestre)
    }
    
    if (input$destino != "Todos") {
      datos <- datos %>%
        filter(Destino == input$destino)
    }
    
    if (input$motivo != "Todos") {
      datos <- datos %>%
        filter(Motivo == input$motivo)
    }
    
    datos
  })
  
  # INDICADORES ----------------------------------------------------------
  
  output$total_visitantes <- renderText({
    
    format(
      nrow(datos_filtrados()),
      big.mark = ".",
      decimal.mark = ","
    )
  })
  
  output$estadia_promedio <- renderText({
    
    paste0(
      round(
        mean(datos_filtrados()$Estadia),
        1
      ),
      " días"
    )
  })
  
  output$gasto_promedio <- renderText({
    
    paste0(
      "$ ",
      round(
        mean(datos_filtrados()$GastoDiario),
        2
      )
    )
  })
  
  output$principal_destino <- renderText({
    
    datos <- datos_filtrados()
    
    validate(
      need(nrow(datos) > 0, "Sin datos")
    )
    
    datos %>%
      count(Destino, sort = TRUE) %>%
      slice_head(n = 1) %>%
      pull(Destino)
  })
  
  # EVOLUCIÓN ANUAL ------------------------------------------------------
  
  output$grafico_anual <- renderPlot({
    
    datos <- datos_filtrados()
    
    validate(
      need(nrow(datos) > 0, "No hay observaciones para los filtros seleccionados")
    )
    
    datos %>%
      group_by(Anio) %>%
      summarise(
        gasto_promedio = mean(GastoDiario),
        .groups = "drop"
      ) %>%
      ggplot(
        aes(
          x = Anio,
          y = gasto_promedio
        )
      ) +
      geom_line(
        color = "steelblue",
        linewidth = 1.2
      ) +
      geom_point(
        color = "steelblue",
        size = 3
      ) +
      scale_x_continuous(
        breaks = sort(unique(datos$Anio))
      ) +
      scale_y_continuous(
        labels = label_number(
          decimal.mark = ",",
          big.mark = "."
        )
      ) +
      labs(
        x = "Año",
        y = "Gasto diario promedio"
      ) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank()
      )
  })
  
  # GASTO POR TRIMESTRE --------------------------------------------------
  
  output$grafico_trimestre <- renderPlot({
    
    datos <- datos_filtrados()
    
    validate(
      need(nrow(datos) > 0, "No hay observaciones para los filtros seleccionados")
    )
    
    datos %>%
      group_by(Trimestre) %>%
      summarise(
        gasto_promedio = mean(GastoDiario),
        .groups = "drop"
      ) %>%
      mutate(
        Trimestre = factor(
          Trimestre,
          levels = c("T1", "T2", "T3", "T4")
        )
      ) %>%
      ggplot(
        aes(
          x = Trimestre,
          y = gasto_promedio
        )
      ) +
      geom_col(
        fill = "steelblue",
        width = 0.7
      ) +
      geom_text(
        aes(
          label = round(gasto_promedio, 1)
        ),
        vjust = -0.5,
        size = 4
      ) +
      scale_y_continuous(
        expand = expansion(mult = c(0, 0.12))
      ) +
      labs(
        x = "Trimestre",
        y = "Gasto diario promedio"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # PRINCIPALES DESTINOS -------------------------------------------------
  
  output$grafico_destinos <- renderPlot({
    
    datos_filtrados() %>%
      count(Destino, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(
        aes(
          x = reorder(Destino, n),
          y = n
        )
      ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(
        labels = label_number(
          big.mark = ".",
          decimal.mark = ","
        )
      ) +
      labs(
        x = "Destino",
        y = "Cantidad de observaciones"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # MOTIVOS DEL VIAJE ----------------------------------------------------
  
  output$grafico_motivos <- renderPlot({
    
    datos_filtrados() %>%
      count(Motivo, sort = TRUE) %>%
      ggplot(
        aes(
          x = reorder(Motivo, n),
          y = n
        )
      ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(
        labels = label_number(
          big.mark = ".",
          decimal.mark = ","
        )
      ) +
      labs(
        x = "Motivo del viaje",
        y = "Cantidad de observaciones"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # MOTIVO SEGÚN DESTINO -------------------------------------------------
  
  output$grafico_destino_motivo <- renderPlot({
    
    destinos_principales <- datos_filtrados() %>%
      count(Destino, sort = TRUE) %>%
      slice_head(n = 8) %>%
      pull(Destino)
    
    datos_filtrados() %>%
      filter(Destino %in% destinos_principales) %>%
      ggplot(
        aes(
          x = Destino,
          fill = Motivo
        )
      ) +
      geom_bar(position = "fill") +
      coord_flip() +
      scale_y_continuous(
        labels = label_percent(
          accuracy = 1,
          decimal.mark = ","
        )
      ) +
      labs(
        x = "Destino",
        y = "Porcentaje de visitantes",
        fill = "Motivo"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # GASTO SEGÚN DESTINO --------------------------------------------------
  
  output$gasto_destino <- renderPlot({
    
    datos_filtrados() %>%
      group_by(Destino) %>%
      summarise(
        gasto_promedio = mean(GastoDiario),
        observaciones = n(),
        .groups = "drop"
      ) %>%
      filter(observaciones >= 20) %>%
      arrange(desc(gasto_promedio)) %>%
      slice_head(n = 10) %>%
      ggplot(
        aes(
          x = reorder(Destino, gasto_promedio),
          y = gasto_promedio
        )
      ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        x = "Destino",
        y = "Gasto diario promedio"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # GASTO SEGÚN MOTIVO ---------------------------------------------------
  
  output$gasto_motivo <- renderPlot({
    
    datos_filtrados() %>%
      group_by(Motivo) %>%
      summarise(
        gasto_promedio = mean(GastoDiario),
        observaciones = n(),
        .groups = "drop"
      ) %>%
      filter(observaciones >= 20) %>%
      ggplot(
        aes(
          x = reorder(Motivo, gasto_promedio),
          y = gasto_promedio
        )
      ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(
        x = "Motivo del viaje",
        y = "Gasto diario promedio"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # RUBROS DEL GASTO -----------------------------------------------------
  
  output$grafico_rubros <- renderPlot({
    
    validate(
      need(
        length(variables_rubros) > 0,
        "No se encontraron variables desagregadas de gasto"
      )
    )
    
    datos_filtrados() %>%
      select(all_of(variables_rubros)) %>%
      pivot_longer(
        cols = everything(),
        names_to = "Rubro",
        values_to = "Gasto"
      ) %>%
      group_by(Rubro) %>%
      summarise(
        gasto_acumulado = sum(Gasto, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        porcentaje = gasto_acumulado /
          sum(gasto_acumulado) * 100
      ) %>%
      arrange(desc(porcentaje)) %>%
      ggplot(
        aes(
          x = reorder(Rubro, porcentaje),
          y = porcentaje
        )
      ) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      scale_y_continuous(
        labels = label_percent(
          scale = 1,
          accuracy = 0.1,
          decimal.mark = ","
        )
      ) +
      labs(
        x = "Rubro",
        y = "Participación en el gasto total"
      ) +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank()
      )
  })
  
  # TABLA RESUMEN --------------------------------------------------------
  
  output$tabla_resumen <- renderTable({
    
    datos_filtrados() %>%
      summarise(
        Observaciones = n(),
        `Estadía promedio` = round(mean(Estadia), 2),
        `Gasto total promedio` = round(mean(GastoTotal), 2),
        `Gasto diario promedio` = round(mean(GastoDiario), 2),
        `Gasto diario mediano` = round(median(GastoDiario), 2)
      )
  })
}

shinyApp(
  ui = ui,
  server = server
)