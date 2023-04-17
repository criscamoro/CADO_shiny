# Módulo para visualización de Cuadros de Resumen Estadístico

# UI ----
cre_UI <- function(id, dataset) {
  ns <- NS(id)
  
  fluidRow(box(fluidRow(
    column(4, 
           selectInput(
             inputId = ns('año'),
             label = 'Año:',
             choices = c('Todos', unique(as.character(dataset$año)))
             )
           ),
    column(4,
           selectInput(
             inputId = ns('mes'),
             label = 'Mes:',
             choices = c('Todos', sort(unique(as.integer(dataset$mes))))
             )
           ),
    column(4,
           downloadButton(
             outputId = ns('desc'),
             label = 'Descargar')
           )
    ), width = 12),
    box(
      DT::dataTableOutput(
        outputId = ns('cuadro')
        ), width = 12)
    )
}

# Server ----
cre_server <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      
      cre <- reactive({
        data <- dataset
        if (input$año != 'Todos') {
          data <- data[data$año == input$año,]
        }
        if (input$mes != 'Todos') {
          data <- data[data$mes == input$mes,]
        }
        data <- st(data %>% select(!c(fecha, año, mes, est)), 
                   summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)',
                                 'max(x)', '(sd(x, na.rm = T)/mean(x, na.rm = T)*100)')),
                   summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
                   out = 'return')
        })
      
      output$cuadro <- DT::renderDataTable(DT::datatable({
        cre()
        }, options = list(dom = 't', pageLength = 45))
        )
      
      output$desc <- downloadHandler(
        filename = function () {
          paste(id, input$año, input$mes, 'resumen.csv', sep = '_')
        },
        content = function(file) {
          write.csv(cre(), file, row.names = F)
        })
    })
}