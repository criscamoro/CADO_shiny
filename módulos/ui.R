#### Definir interfaz gráfica ####
# Interfaz de la barra lateral
sb.menu <- dashboardSidebar(
  sidebarMenu(id = 'tabs',
    menuItem('Inicio', tabName = 'inicio', icon = icon('house')),
    sb.ca('Laguna de Cajititlán', 'caji'),
    sb.ca('Laguna de Zapotlán', 'zapo'),
    sb.ca('Río Santiago', 'santi'),
    sb.ca('Río Zula - Lerma', 'lerma'),
    sb.ca('Río Verde', 'verde'),
    menuItem('Acerca de', tabName = 'about', icon = icon('circle-question')),
    menuItem('Contacto', tabName = 'contacto', icon = icon('envelope'))))

# Cuadros de resumen estadístico
cre.input <- box(fluidRow(
  column(4, selectInput('año', 'Año:', c('Todos', unique(as.character(caji.amb.tidy$año))))),
  column(4, selectInput('mes', 'Mes:', c('Todos', sort(unique(as.integer(caji.amb.tidy$mes)))))),
  column(4, downloadButton('dl.cre', 'Descargar'))),
  width = 12)

cre.output <- box(DT::dataTableOutput('cuadro', width = '100%'), width = 12)

# Series temporales
gst.input <- box(fluidRow(
  column(9, selectizeInput('vars', 'Parámetros', choices = unique(as.character(colnames(caji.amb.tidy)[1:45])), 
                           multiple = T, options = list(plugins = list('remove_button')))), 
  column(3, checkboxInput('stm', label = 'Estandarizar datos*'))), width = 8)

gst.input2 <- box(fluidRow(
  column(6, airDatepickerInput('rango', label = 'Periodo', range = T, language = 'es', 
                               value = c(min(caji.amb.tidy$fecha), max(caji.amb.tidy$fecha)), 
                               minDate = min(caji.amb.tidy$fecha), maxDate = max(caji.amb.tidy$fecha),
                               view = 'months', minView = 'months', 
                               dateFormat = 'yyyy/MM', separator = '-')), 
  column(6, downloadButton('dl.gst', 'Descargar'))), width = 4)

gst.output <- fluidRow(box(
  plotOutput('gst.plot'), width = 12))

gst.stm <- box(width = 12, 
               '*Si se selecciona, los datos serán reescalados a una media de 0 y desviación estándar de 1')

source('módulos/lerma_ui.R')

# Página de Inicio (Parámetros ambientales monitoreo)
#Temperatura, OD, pH, Dureza, Clorofila, Nitrógeno total, Coliformes, Profundidad

#### Contenido ####

sb.body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'inicio', box(includeMarkdown('C:/Users/ccopi/Desktop/PP/Caji/README.md'), width = 12)),
    tabItem(tabName = 'caji_cre', fluidRow(cre.input), fluidRow(cre.output)),
    tabItem(tabName = 'caji_gst', fluidRow(gst.input, gst.input2), gst.output, fluidRow(gst.stm)),
    tabItem(tabName = 'zapo_cre', box(uiOutput('a'))),
    tabItem(tabName = 'zapo_gst', box(uiOutput('a'))),
    tabItem(tabName = 'santi_cre', 'Datos del Río Santiago'),
    tabItem(tabName = 'santi_gst', 'Datos del Río Santiago 2'),
    tabItem(tabName = 'lerma_cre', fluidRow(lerma.cre.input), fluidRow(lerma.cre.output)),
    tabItem(tabName = 'lerma_gst', fluidRow(lerma.gst.input, lerma.gst.input2), lerma.gst.output, fluidRow(gst.stm)),
    tabItem(tabName = 'verde_cre', 'Datos del Río Verde'),
    tabItem(tabName = 'verde_gst', 'Datos del Río Verde 2'),
    tabItem(tabName = 'about', box(includeMarkdown('C:/Users/ccopi/Desktop/PP/Caji/README.md'), width = 12)),
    tabItem(tabName = 'contacto', 'cristofer.camarena@alumnos.udg.mx')))

ui <- dashboardPage(
  dashboardHeader(titleWidth = 0),
  sb.menu,
  sb.body)