# Aplicación de visualización de datos del Programa de Monitoreo de Cuerpos de Agua de Occidente (CAO)

library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(vtable)
library(DT)
library(shinyWidgets)
library(ggtext)
library(vegan)

amb.tidy <- read_csv('C:/Users/ccopi/Desktop/PP/Caji/datos/ambiental_tidy.csv') ### pendiente
amb.rect <- read_csv('C:/Users/ccopi/Desktop/PP/Caji/datos/rectangulares/ambiental_rect.csv') ### pendiente

#### Definir interfaz gráfica ####
# Interfaz de la barra lateral
sb.menu <- dashboardSidebar(
  sidebarMenu(
    menuItem('Inicio', tabName = 'inicio', icon = icon('house')),
    menuItem('Cuerpo de Agua', tabName = 'cda', icon = icon('water'),
             menuSubItem('Lago de Chapala', tabName = 'chapala'),
             menuSubItem('Laguna de Cajititlán', tabName = 'caji'),
             menuSubItem('Laguna de Zapotlán', tabName = 'zapo'),
             menuSubItem('Río Santiago', tabName = 'santi'),
             menuSubItem('Río Zula - Lerma', tabName = 'lerma'),
             menuSubItem('Río Verde', tabName = 'verde')),
    menuItem('Acerca de', tabName = 'about', icon = icon('circle-question')),
    menuItem('Contancto', tabName = 'contacto', icon = icon('envelope'))))

#Cuadros de resumen estadístico
cr.input <- box(fluidRow(
  column(4, selectInput('año', 'Año:', c('Todos', unique(as.character(amb.tidy$año))))),
  column(4, selectInput('mes', 'Mes:', c('Todos', sort(unique(as.integer(amb.tidy$mes)))))),
  column(4, downloadButton('dl.cre', 'Descargar'))),
  width = 12)

cr.output <- box(DT::dataTableOutput('cuadro', width = '100%'), width = 12)

# Series temporales
st.input <- box(fluidRow(
  column(9, selectizeInput('vars', 'Parámetros', choices = unique(as.character(amb.rect$idParametro)), 
                           multiple = T, options = list(plugins = list('remove_button')))), 
  column(3, checkboxInput('stm', label = 'Estandarizar datos*'))), width = 8)

st.input2 <- box(fluidRow(
  column(6, airDatepickerInput('rango', label = 'Periodo', range = T, language = 'es', 
                           value = c(min(amb.tidy$fecha), max(amb.tidy$fecha)), 
                           minDate = min(amb.tidy$fecha), maxDate = max(amb.tidy$fecha),
                           view = 'months', minView = 'months', 
                           dateFormat = 'yyyy/MM', separator = '-')), 
  column(6, downloadButton('dl.gst', 'Descargar'))), width = 4)

st.output <- fluidRow(box(
  plotOutput('st.plot'), width = 12))

st.stm <- box(width = 12, 
              '*Si se selecciona, los datos serán reescalados a una media de 0 y desviación estándar de 1')

# Página de Inicio (Parámetros ambientales monitoreo)
#Temperatura, OD, pH, Dureza, Clorofila, Nitrógeno total, Coliformes, Profundidad

#### Contenido ####
sb.body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'inicio', box(includeMarkdown('C:/Users/ccopi/Desktop/PP/Caji/README.md'), width = 12)),
    tabItem(tabName = 'chapala', fluidRow(st.input, st.input2), st.output, fluidRow(st.stm)),
    tabItem(tabName = 'caji', fluidRow(cr.input), fluidRow(cr.output)),
    tabItem(tabName = 'zapo', 'Datos de la Laguna de Zapotlán'),
    tabItem(tabName = 'santi', 'Datos del Río Santiago'),
    tabItem(tabName = 'lerma', 'Datos del Río Zula - Lerma'),
    tabItem(tabName = 'verde', 'Datos del Río Verde'),
    tabItem(tabName = 'about', box(includeMarkdown('C:/Users/ccopi/Desktop/PP/Caji/README.md'), width = 12)),
    tabItem(tabName = 'contacto', 'cristofer.camarena@alumnos.udg.mx')))

ui <- dashboardPage(
                dashboardHeader(titleWidth = 0),
                sb.menu,
                sb.body)

#### Server ####
server <- function(input, output) {
  
  # Cargar datos
  
  # Cuadros de resumen estadístico
  amb.tidy.cre <- reactive({
    data <- amb.tidy
    if (input$año != 'Todos') {
      data <- data[data$año == input$año,]
    }
    if (input$mes != 'Todos') {
      data <- data[data$mes == input$mes,]
    }
    data <- st(data %>% select(1:45), 
               summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)',
                             'max(x)', '(sd(x, na.rm = T)/mean(x, na.rm = T)*100)')),
               summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
               out = 'return')
  }) 
  
  output$cuadro <- DT::renderDataTable(DT::datatable({
    amb.tidy.cre()
  }, options = list(dom = 't', pageLength = 45)))
  
  output$dl.cre <- downloadHandler(
    filename = function () {
      paste('resumen', '_', input$año, '_', input$mes, '.csv', sep = '')
      },
    content = function(file){
      write.csv(amb.tidy.cre(), file, row.names = F)
    }
  )
  # Gráficos de series temporales
  col.vec <- reactive({
    col.vec <- if (length(input$vars) == 0) {scales::hue_pal()(1)
    } else {
      scales::hue_pal()(sum(lengths(input$vars)))
      }
    })
  
  amb.tidy.gst <- reactive({
    ggplot(data = if(input$stm == T) {
      amb.tidy %>% 
        mutate(across(Temperatura:Clorofilas,
                      ~decostand(., method = 'standardize', na.rm = T))) %>% 
        filter(fecha < max(input$rango) & fecha > min(input$rango))
    } else {
      amb.tidy %>% 
        filter(fecha < max(input$rango) & fecha > min(input$rango))
    }, 
    aes(x = fecha)) +
      lapply(input$vars, function(x){
        geom_line(aes(y = .data[[x]], color = x))
      }) +
      scale_color_manual(name = 'Parámetro', values = col.vec()) + 
      labs(title = paste(input$vars, collapse = ', '),
           x = 'fecha', y = 'valor') +
      theme_classic() + 
      theme(plot.title = element_textbox_simple(halign = 0.5, margin = unit(c(5, 0, 0, 5), 'pt')))
  })

  output$st.plot <- renderPlot({
    amb.tidy.gst()
    })
  
  output$dl.gst <- downloadHandler(
    filename = function(){
      paste(input$vars, '_', input$rango, '.png')
    },
    content = function(file){
      ggsave(file, plot = amb.tidy.gst(), 
             width = 1920, height = 1080, units = 'px', pointsize = 12, bg = 'white',dpi = 300)
    }
  )
}

shinyApp(ui = ui, server = server)
