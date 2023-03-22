# Aplicación de visualización de datos del Programa de Monitoreo de Cuerpos de Agua de Occidente (CAO)

library(shiny)
library(shinythemes)
library(shinydashboard)

# Definir interfaz gráfica
ui <- dashboardPage(
                dashboardHeader(title = 'CAO'),
                dashboardSidebar(
                  sidebarMenu(
                    menuItem('Inicio', tabName = 'inicio', icon = icon('house')),
                    menuItem('Cuerpo de Agua', tabName = 'cda', icon = icon('water'),
                             menuSubItem('Lago de Chapala', tabName = 'chapala'),
                             menuSubItem('Laguna de Cajititlán', tabName = 'caji'),
                             menuSubItem('Laguna de Zapotlán', tabName = 'zapo'),
                             menuSubItem('Río Santiago', tabName = 'santi'),
                             menuSubItem('Río Zula - Lerma', tabName = 'lerma'),
                             menuSubItem('Río Verde', tabName = 'verde')),
                    menuItem('Contancto', tabName = 'contacto', icon = icon('envelope'))
                    )
                  ),
                dashboardBody(
                  tabItems(
                    tabItem(tabName = 'inicio', includeMarkdown('C:/Users/ccopi/Desktop/PP/Caji/README.md')),
                    tabItem(tabName = 'chapala', 'Datos del Lago de Chapala'),
                    tabItem(tabName = 'caji', 'Datos de la Laguna de Cajititlán'),
                    tabItem(tabName = 'zapo', 'Datos de la Laguna de Zapotlán'),
                    tabItem(tabName = 'santi', 'Datos del Río Santiago'),
                    tabItem(tabName = 'lerma', 'Datos del Río Zula - Lerma'),
                    tabItem(tabName = 'verde', 'Datos del Río Verde'),
                    tabItem(tabName = 'contacto', 'cristofer.camarena@alumnos.udg.mx')
                  )
                )
)



server <- function(input, output) {
}

# Run the application 
shinyApp(ui = ui, server = server)
