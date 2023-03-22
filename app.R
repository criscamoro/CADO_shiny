# Aplicación de visualización de datos del Programa de Monitoreo de Cuerpos de Agua de Occidente (CAO)

library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(vtable)
library(DT)

amb.tidy <- read_csv('C:/Users/ccopi/Desktop/PP/Caji/datos/ambiental_tidy.csv')
amb.rect <- read_csv('C:/Users/ccopi/Desktop/PP/Caji/datos/rectangulares/ambiental_rect.csv')

cv <- function(x) {
  c.v <- (sd(x, na.rm = T)/mean(x, na.rm = T)*100)
}

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
    menuItem('Contancto', tabName = 'contacto', icon = icon('envelope'))))

#Cuadros de resumen estadístico
cr.input <- box(fluidRow(
  column(6, selectInput('año', 'Año:', c('Todos', unique(as.character(amb.tidy$año))))),
  column(6, selectInput('mes', 'Mes:', c('Todos', sort(unique(as.integer(amb.tidy$mes))))))),
  width = 12)

cr.output <- box(DT::dataTableOutput('cuadro', width = '100%'), width = 12)

# Contenido
sb.body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'inicio', box(includeMarkdown('C:/Users/ccopi/Desktop/PP/Caji/README.md'), width = 12)),
    tabItem(tabName = 'chapala', 'Datos de Chapala'),
    tabItem(tabName = 'caji', fluidRow(cr.input), fluidRow(cr.output)),
    tabItem(tabName = 'zapo', 'Datos de la Laguna de Zapotlán'),
    tabItem(tabName = 'santi', 'Datos del Río Santiago'),
    tabItem(tabName = 'lerma', 'Datos del Río Zula - Lerma'),
    tabItem(tabName = 'verde', 'Datos del Río Verde'),
    tabItem(tabName = 'contacto', 'cristofer.camarena@alumnos.udg.mx')))

ui <- dashboardPage(
                dashboardHeader(titleWidth = 0),
                sb.menu,
                sb.body)

#### Server ####
server <- function(input, output) {
  output$cuadro <- DT::renderDataTable(DT::datatable({
    data <- amb.tidy
    if (input$año != 'Todos') {
      data <- data[data$año == input$año,]
    }
    if (input$mes != 'Todos') {
      data <- data[data$mes == input$mes,]
    }
    data <- st(data %>% select(1:45), 
               summ = list(c('notNA(x)', 'mean(x)', 'sd(x)', 'min(x)', 'max(x)', 'cv(x)')),
               summ.names = list(c('N', 'Media', 'D.E.', 'Min', 'Max', 'C.V.')),
               out = 'return')
  }, options = list(dom = 't', pageLength = 45)))
}

shinyApp(ui = ui, server = server)
