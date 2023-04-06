#Función para generar los cuadros de resumen estadísticos
id.data <- function(x) {
  if(x == 'caji') {
    return(caji.amb.tidy)
  }
  if(x == 'lerma') {
    return(lerma.amb.tidy)
  }
  if(x == 'zapo') {
    return(zapo.amb.tidy)
  }
  if(x == 'verde') {
    return(verde.amb.tidy)
  }
  if(x == 'santi') {
    return(santi.amb.tidy)
  }
}

cre <- reactive({
  data <- id.data(first(first(str_split(input$tabs, '_'))))
  if (input$año != 'Todos') {
    data <- data[data$año == input$año,]
  }
  if (input$mes != 'Todos') {
    data <- data[data$mes == input$mes,]
  }
  data <- st(data %>% select(1:45), 
             summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)',
                           'max(x)', 'cv(x)')),
             summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
             out = 'return')
}) 

