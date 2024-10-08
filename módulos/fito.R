# Módulo de visualización de datos de fitoplancton de la Laguna de Cajititlán

# UI ----
fito_UI <- function(id, dataset) {
  ns <- NS(id)
  
  fluidRow(
    box(fluidRow(
      column(
        9,
        selectInput(
          inputId = ns("nvl"),
          label = "Nivel taxonómico",
          choices = c("Phylum", "Clase", "Orden", "Familia", "Género")
        )
      ),
      column(
        3,
        checkboxInput(
          inputId = ns("agr"),
          label = "Agrupar por año"
          )
        )
      ), width = 8),
    box(
      column(
        12,
        airDatepickerInput(
          inputId = ns("rango"),
          label = "Periodo",
          value = c(min(dataset$fecha), max(dataset$fecha)), minDate = min(dataset$fecha), maxDate = max(dataset$fecha),
          range = T, language = "es", dateFormat = "yyyy/MM", separator = "-", view = "months", minView = "months"
          )
        ),
      width = 4
      ),
    box(
      plotOutput(
        outputId = ns("treemap_plot")
        ),
      width = 12
      ),
    box(
      plotOutput(
        outputId = ns("ab_n_plot")
        ),
      width = 6
      ),
    box(
      plotOutput(
        outputId = ns("sobs_n_plot")
        ),
      width = 6
      ),
    box(
      plotOutput(
        outputId = ns("ab_f_plot")
        ),
      width = 6
      ),
    box(
      plotOutput(
        outputId = ns("sobs_f_plot")
        ),
      width = 6
      ),
    box(
      plotOutput(
        outputId = ns("shannon_plot")
        ),
      width = 6
      ),
    box(
      plotOutput(
        outputId = ns("pcoa_plot")
        ),
      width = 6
      )
    )
}

# Server ----

fito_server <- function(id, dataset) {
  moduleServer(
    id,
    function(input, output, session) {
      treemap <- reactive({
        ggplot(data = dataset %>% 
                 group_by(.data[[input$nvl]]) %>% 
                 summarise(conteo = sum(conteo)),
               aes(area = conteo, fill = .data[[input$nvl]],
                   label = paste(.data[[input$nvl]], "\n", conteo, " cel/ml", sep = ""))) +
          geom_treemap(layout = "srow") +
          geom_treemap_text(color = "white", place = "centre", size = 22, layout = "srow")
        })
      
      ab_n <- reactive({
        ggplot(
          data = if(input$agr == T) {
            dataset %>% 
              group_by(año) %>% 
              summarise(conteo = sum(conteo)) 
          } else {
            dataset %>% 
              group_by(fecha) %>% 
              summarise(conteo = sum(conteo)) %>% 
              filter(fecha < max(input$rango) & fecha > min(input$rango))
          },
          aes(
            x = if (input$agr == T) {
              año
            } else {
              fecha
            },
            y = conteo
          )
        ) +
          geom_line(aes(group = 1)) +
          geom_point(color = "#009999", size = 5) +
          labs(
            title = "Abundancia", 
            x = "Periodo", 
            y = "células/ml") +
          scale_x_discrete(guide = guide_axis(check.overlap = T)) +
          theme_classic() + 
          theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 0, 5), "pt")))
      })
      
      ab_f <- reactive({
        ggplot(
          data = if (input$agr == T) {
            dataset %>% 
              group_by(año) %>% 
              mutate(ni = (conteo/sum(conteo))*100)
          } else {
            dataset %>% 
              group_by(fecha) %>% 
              mutate(ni = (conteo/sum(conteo))*100) %>% 
              filter(fecha < max(input$rango) & fecha > min(input$rango))
            },
          aes(
            x = if (input$agr == T) {
              año
            } else {
              fecha
            },
            y = ni,
            fill = .data[[input$nvl]],
            color = .data[[input$nvl]]
          )
        ) +
          geom_bar(stat = "identity") +
          labs(
            title = paste("Contribución de ", input$nvl, "s", " a la abundancia", sep = ""),
            x = "Periodo", 
            y = "%"
          ) +
          scale_x_discrete(guide = guide_axis(check.overlap = T)) +
          theme_classic() +
          theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 0, 5), "pt")))
      })
      
      sobs_n <- reactive({
        ggplot(
          data = if(input$agr == T) {
            dataset %>% 
              group_by(año) %>% 
              filter(conteo > 0) %>% 
              summarise(S = length(unique(taxa)))
          } else {
            dataset %>% 
              group_by(fecha) %>% 
              filter(conteo > 0) %>% 
              summarise(S = length(unique(taxa))) %>% 
              filter(fecha < max(input$rango) & fecha > min(input$rango))
          },
          aes(
            x = if(input$agr == T) {
              año
            } else {
              fecha
            },
            y = S
          )
        ) +
          geom_line(aes(group = 1)) +
          geom_point(color = "#999900", size = 5) +
          labs(
            title = "Riqueza de especies",
            x = "Periodo",
            y = "Sobs"
          ) +
          scale_x_discrete(guide = guide_axis(check.overlap = T)) +
          theme_classic() +
          theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 0, 5), "pt")))
      })
      
      sobs_f <- reactive({
        ggplot(
          data = if(input$agr == T) {
           dataset %>% 
              group_by(año) %>% 
              filter(conteo > 0) %>% 
              distinct(taxa, .keep_all = T) %>% 
              mutate(v_a = 1) %>% 
              group_by(año, .data[[input$nvl]]) %>% 
              summarise(v_a = sum(v_a)) %>% 
              group_by(año) %>% 
              mutate(`%` = (v_a/sum(v_a))*100)
          } else {
            dataset %>% 
              group_by(fecha) %>% 
              filter(conteo > 0) %>% 
              distinct(taxa, .keep_all = T) %>% 
              mutate(v_a = 1) %>% 
              group_by(fecha, .data[[input$nvl]]) %>% 
              summarise(v_a = sum(v_a)) %>% 
              group_by(fecha) %>% 
              mutate(`%` = (v_a/sum(v_a))*100) %>% 
            filter(fecha < max(input$rango) & fecha > min(input$rango))
          },
          aes(
            x = if(input$agr == T) {
              año
            } else {
              fecha
            },
            y = `%`, 
            fill = .data[[input$nvl]], 
            color = .data[[input$nvl]]
          )
        ) +
          geom_bar(stat = "identity") +
          labs(
            title = paste("Contribución de ", input$nvl, "s ", "a la riqueza", sep = ""),
            x = "Periodo", 
            y = "%"
              ) +
          scale_x_discrete(guide = guide_axis(check.overlap = T)) +
          theme_classic() +
          theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 0, 5), "pt")))
      })
      
      caji_fito_sqrt2bray <- vegdist(caji_fito_tidy %>% 
                                       group_by(año) %>% 
                                       summarise(across(1:61, ~sqrt(sqrt(sum(.))))) %>% 
                                       select(!c(año)), method = "bray")
      
      caji_fito_pcoa <- cmdscale(caji_fito_sqrt2bray, eig = T)
      
      pcoa_año_plot <- ggplot(suppressWarnings(as_tibble(caji_fito_pcoa$points)) %>% 
                                mutate(año = unique(caji_fito_tidy$año)), 
                              aes(x = V1, y = V2, label = año)) + 
        geom_point(aes(size = 6, color = "green")) +
        geom_text(vjust = -0.6) +
        geom_line(arrow = arrow(length = unit(0.5, "cm"), type = "closed")) +
        labs(title = "PCoA del fitoplancton de la Laguna de Cajititlán de 2014 a 2019", 
             x = paste("PCoA 1 (", round(caji_fito_pcoa$eig[1]/sum(caji_fito_pcoa$eig) * 100, digits = 2), "%)", sep = ""), 
             y = paste("PCoA 2 (", round(caji_fito_pcoa$eig[2]/sum(caji_fito_pcoa$eig) * 100, digits = 2), "%)", sep = "")) +
        scale_x_reverse() +
        theme(
          plot.title = element_text(hjust = 0.5),
          legend.position = "none", 
          axis.line = element_line(color = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(),
          panel.background = element_blank()
        ) 
      
      shannon <- function(x) {
        f <- (x[x > 0]/sum(x))
        -sum(f * log(f, 2))
      }
      
      sh <- reactive({
        ggplot(
          data = if(input$agr == T) {
            dataset %>% 
              group_by(año, taxa) %>% 
              summarise(conteo = sum(conteo)) %>% 
              group_by(año) %>% 
              summarise(`H'` = shannon(conteo)) %>% 
              mutate(Periodo = año)
          } else {
            dataset %>% 
              group_by(fecha, taxa) %>% 
              summarise(conteo = sum(conteo)) %>% 
              group_by(fecha) %>% 
              summarise(`H'` = shannon(conteo)) %>% 
              filter(fecha < max(input$rango) & fecha > min(input$rango)) %>% 
              mutate(Periodo = fecha)
          },
          aes(
            x = Periodo,
            y = `H'`
          )
        ) +
          geom_line(aes(group = 1)) +
          geom_point(color = "#009933", size = 5) +
          labs(
            title = "Diversidad del fitoplancton de la Laguna de Cajititlán",
            x = "Periodo",
            y = "bits/ind"
          ) +
          scale_x_discrete(guide = guide_axis(check.overlap = T)) +
          theme_classic() +
          theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 0, 5), "pt")))
      })
      
      output$treemap_plot <- renderPlot({
        treemap()
      })
      
      output$ab_n_plot <- renderPlot({
        ab_n()
      })
      
      output$ab_f_plot <- renderPlot({
        ab_f()
      })
      
      output$sobs_n_plot <- renderPlot({
        sobs_n()
      })
      
      output$sobs_f_plot <- renderPlot({
        sobs_f()
      })
      
      output$pcoa_plot <- renderPlot({
        pcoa_año_plot
      })
      
      output$shannon_plot <- renderPlot({
        sh()
      })
    }
  )
}
