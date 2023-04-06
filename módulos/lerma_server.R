# CRE
lerma.amb.tidy.cre <- reactive({
  data1 <- lerma.amb.tidy
  if (input$lerma.año != 'Todos') {
    data1 <- data1[data1$año == input$lerma.año,]
  }
  if (input$lerma.mes != 'Todos') {
    data1 <- data1[data1$mes == input$lerma.mes,]
  }
  data1 <- st(data1 %>% select(1:45), 
             summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)',
                           'max(x)', 'cv(x)')),
             summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
             out = 'return')
})  

output$lerma.cuadro <- DT::renderDataTable(DT::datatable({
  lerma.amb.tidy.cre()
}, options = list(dom = 't', pageLength = 45)))

output$lerma.dl.cre <- downloadHandler(
  filename = function () {
    paste('resumen', '_', input$lerma.año, '_', input$lerma.mes, '.csv', sep = '')
  },
  content = function(file){
    write.csv(lerma.amb.tidy.cre(), file, row.names = F)
  }
)

# Gráficos de series temporales
col.vec <- reactive({
  col.vec <- if (length(input$lerma.vars) == 0) {scales::hue_pal()(1)
  } else {
    scales::hue_pal()(sum(lengths(input$lerma.vars)))
  }
})

lerma.amb.tidy.gst <- reactive({
  ggplot(data = if(input$lerma.stm == T) {
    lerma.amb.tidy %>% 
      mutate(across(select(-c(fecha, año, mes, est)),
                    ~decostand(., method = 'standardize', na.rm = T))) %>% 
      filter(fecha < max(input$lerma.rango) & fecha > min(input$lerma.rango))
  } else {
    lerma.amb.tidy %>% 
      filter(fecha < max(input$lerma.rango) & fecha > min(input$lerma.rango))
  }, 
  aes(x = fecha)) +
    lapply(input$lerma.vars, function(x){
      geom_line(aes(y = .data[[x]], color = x))
    }) +
    scale_color_manual(name = 'Parámetro', values = col.vec()) + 
    labs(title = paste(input$lerma.vars, collapse = ', '),
         x = 'fecha', y = 'valor') +
    theme_classic() + 
    theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 0, 5), 'pt')))
})

output$lerma.gst.plot <- renderPlot({
  lerma.amb.tidy.gst()
})

output$dl.gst <- downloadHandler(
  filename = function(){
    paste(input$lerma.vars, '_', input$lerma.rango, '.png')
  },
  content = function(file){
    ggsave(file, plot = amb.tidy.gst(), 
           width = 1920, height = 1080, units = 'px', pointsize = 12, bg = 'white',dpi = 300)
  }
)