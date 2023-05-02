# Módulo para visualización de Gráficas de Series Temporales (gst)

# UI ----
gst_UI <- function(id, dataset) {
  ns <- NS(id)

  fluidRow(
    box(fluidRow(
      column(
        9,
        selectizeInput(
          inputId = ns("vars"),
          label = "Parámetros",
          choices = colnames(dataset %>% select(!c(fecha, año, mes, est))),
          options = list(plugins = list("remove_button")),
          multiple = T
        )
      ),
      column(
        3,
        checkboxInput(
          inputId = ns("stm"),
          label = "Estandarizar datos*"
        )
      )
    ), width = 8),
    box(
      column(
        6,
        airDatepickerInput(
          inputId = ns("rango"),
          label = "Periodo",
          value = c(min(dataset$fecha), max(dataset$fecha)), minDate = min(dataset$fecha), maxDate = max(dataset$fecha),
          range = T, language = "es", dateFormat = "yyyy/MM", separator = "-", view = "months", minView = "months"
        )
      ),
      column(
        6,
        downloadButton(
          outputId = ns("desc"),
          label = "Descargar"
        )
      ),
      width = 4
    ),
    box(
      plotOutput(
        outputId = ns("plot")
      ),
      width = 12
    ),
    box(
      width = 12,
      "*Si se selecciona, los datos serán reescalados a una media de 0 y desviación estándar de 1"
    )
  )
}

# Server ----
gst_server <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      col_vec <- reactive({
        if (length(input$vars) == 0) {
          scales::hue_pal()(1)
        } else {
          scales::hue_pal()(sum(lengths(input$vars)))
        }
      })

      gst <- reactive({
        ggplot(
          data = if (input$stm == T) {
            dataset %>%
              mutate(across(
                !c(fecha, año, mes, est),
                ~ decostand(., method = "standardize", na.rm = T)
              )) %>%
              filter(fecha < max(input$rango) & fecha > min(input$rango))
          } else {
            dataset %>%
              filter(fecha < max(input$rango) & fecha > min(input$rango))
          },
          aes(x = fecha)
        ) +
          lapply(input$vars, function(x) {
            geom_line(aes(y = .data[[x]], color = x))
          }) +
          scale_color_manual(name = "Parámetro", values = col_vec()) +
          labs(
            title = paste(input$vars, collapse = ", "),
            x = "fecha", y = "valor"
          ) +
          theme_classic() +
          theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 0, 5), "pt")))
      })

      output$plot <- renderPlot({
        gst()
      })

      output$desc <- downloadHandler(
        filename = function() {
          paste(id, input$vars, input$rango, "gst.png", sep = "_")
        },
        content = function(file) {
          ggsave(file,
            plot = gst(),
            width = 1920, height = 1080, units = "px", pointsize = 12, bg = "white", dpi = 300
          )
        }
      )
    }
  )
}
