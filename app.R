
# TABLERO SUMATRA

library(shiny)
library(shinydashboard)
library(ggplot2)
library(scales)   

# Traigo datos ya limpios y funciones de métricas
source("R/Lectura_Limpieza.R", local = TRUE)  
source("R/metricas.R",        local = TRUE)   

# ------ UI ------
ui <- dashboardPage(
  dashboardHeader(title = "Sumatra Viajes"),
  dashboardSidebar(
    # Filtro por fecha
    dateRangeInput(
      "f_fecha", "Fecha (fecha_ref):",
      start = fecha_min, end = fecha_max,
      min   = fecha_min, max  = fecha_max
    ),
    # Filtro por zona (checkbox)
    checkboxGroupInput(
      "f_zona", "Zona:",
      choices  = zonas_choices,
      selected = zonas_choices
    ),
    # Botoncitos útiles
    div(style = "display:flex; gap:6px; margin-bottom:10px;",
        actionButton("z_all",  "Todas"),
        actionButton("z_none", "Ninguna")
    ),
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
              # KPIs
              fluidRow(
                valueBoxOutput("rate"),
                valueBoxOutput("count"),
                valueBoxOutput("users")
              ),
              # Serie mensual + top vendedores
              fluidRow(
                box(width = 8, status = "info", solidHeader = TRUE,
                    title = "Ventas por mes",
                    plotOutput("plot_serie", height = 400)),
                box(width = 4, status = "info", solidHeader = TRUE,
                    title = "Top 10 vendedores",
                    tableOutput("tbl_vendedores"))
              ),
              # Torta por zona + top destinos (PAX)
              fluidRow(
                box(width = 6, status = "info", solidHeader = TRUE,
                    title = "Top 10 ventas por zona (ML)",
                    plotOutput("plot_zonas", height = 350)),
                box(width = 6, status = "info", solidHeader = TRUE,
                    title = "Top 10 destinos por pasajeros (PAX)",
                    plotOutput("plot_destinos", height = 350))
              )
      )
    )
  )
)

# ------ SERVER ------
server <- function(input, output, session) {
  
  # Botones: seleccionar todas / ninguna zona
  observeEvent(input$z_all,  {
    updateCheckboxGroupInput(session, "f_zona", selected = zonas_choices)
  })
  observeEvent(input$z_none, {
    updateCheckboxGroupInput(session, "f_zona", selected = character(0))
  })
  
  # Base filtrada para todo el tablero
  base_filtrada <- reactive({
    req(input$f_fecha)
    zonas_sel <- if (is.null(input$f_zona) || length(input$f_zona) == 0)
      zonas_choices else input$f_zona
    
    agencia_base |>
      dplyr::filter(
        !is.na(fecha_ref),
        fecha_ref >= input$f_fecha[1],
        fecha_ref <= input$f_fecha[2],
        ID_zona %in% zonas_sel
      )
  })
  
  # ---- KPIs (uso funciones de métricas) ----
  output$rate <- renderValueBox({
    vt <- kpi_ventas_tot(base_filtrada())
    valueBox(format(round(vt, 0), big.mark = ".", decimal.mark = ","),
             "Ventas totales (ML)", icon = icon("dollar-sign"), color = "teal")
  })
  
  output$count <- renderValueBox({
    rt <- kpi_renta_tot(base_filtrada())
    valueBox(format(round(rt, 0), big.mark = ".", decimal.mark = ","),
             "Renta total (ML)", icon = icon("chart-line"), color = "olive")
  })
  
  output$users <- renderValueBox({
    cu <- kpi_clientes_unicos(base_filtrada())
    valueBox(format(cu, big.mark = ".", decimal.mark = ","),
             "Clientes únicos", icon = icon("users"), color = "aqua")
  })
  
  # ---- Serie mensual ----
  output$plot_serie <- renderPlot({
    df <- serie_mensual(base_filtrada())
    ggplot(df, aes(mes, ventas)) +
      geom_col() +
      labs(x = NULL, y = "Ventas (ML)")
  })
  
  # ---- Top vendedores ----
  output$tbl_vendedores <- renderTable({
    tv <- top_vendedores(base_filtrada(), n = 10) |>
      dplyr::mutate(Ventas = format(round(ventas, 0), big.mark = ".", decimal.mark = ",")) |>
      dplyr::select(Vendedor, Ventas)
    tv
  })
  
  # ---- Torta por zona (Top9 + Otros) ----
  output$plot_zonas <- renderPlot({
    dz <- ventas_por_zona_top10(base_filtrada())
    ggplot(dz, aes(x = "", y = ventas, fill = ID_zona)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(title = "Top 10 ventas por zona") +
      theme_void() +
      guides(fill = guide_legend(ncol = 2, title = NULL))
  })
  
  # ---- Top destinos por PAX ----
  output$plot_destinos <- renderPlot({
    dd <- top_destinos_pax(base_filtrada(), n = 10)
    ggplot(dd, aes(x = reorder(De_nombre, paxs), y = paxs)) +
      geom_col() +
      coord_flip() +
      labs(x = NULL, y = "Pasajeros (PAX)") +
      scale_y_continuous(labels = label_number(big.mark = ".", decimal.mark = ",")) +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank())
  })
}

# ------ RUN ------
shinyApp(ui = ui, server = server)
