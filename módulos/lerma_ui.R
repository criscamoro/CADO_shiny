#Cuadros de resumen estadístico
lerma.cre.input <- box(fluidRow(
  column(4, selectInput('lerma.año', 'Año:', c('Todos', unique(as.character(lerma.amb.tidy$año))))),
  column(4, selectInput('lerma.mes', 'Mes:', c('Todos', sort(unique(as.integer(lerma.amb.tidy$mes)))))),
  column(4, downloadButton('lerma.dl.cre', 'Descargar'))),
  width = 12)

lerma.cre.output <- box(DT::dataTableOutput('lerma.cuadro', width = '100%'), width = 12)

# Series temporales
lerma.gst.input <- box(fluidRow(
  column(9, selectizeInput('lerma.vars', 'Parámetros', choices = unique(as.character(colnames(lerma.amb.tidy)[1:45])), 
                           multiple = T, options = list(plugins = list('remove_button')))), 
  column(3, checkboxInput('lerma.stm', label = 'Estandarizar datos*'))), width = 8)

lerma.gst.input2 <- box(fluidRow(
  column(6, airDatepickerInput('lerma.rango', label = 'Periodo', range = T, language = 'es', 
                               value = c(min(lerma.amb.tidy$fecha), max(lerma.amb.tidy$fecha)), 
                               minDate = min(lerma.amb.tidy$fecha), maxDate = max(lerma.amb.tidy$fecha),
                               view = 'months', minView = 'months', 
                               dateFormat = 'yyyy/MM', separator = '-')), 
  column(6, downloadButton('lerma.dl.gst', 'Descargar'))), width = 4)

lerma.gst.output <- fluidRow(box(
  plotOutput('lerma.gst.plot'), width = 12))