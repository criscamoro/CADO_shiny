# Aplicación de visualización de datos del Programa de Monitoreo de Cuerpos de Agua de Occidente (CAO)

# Paquetes ----

library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(openxlsx)
library(qdap)
library(lubridate)
library(vtable)
library(DT)
library(shinyWidgets)
library(ggtext)
library(vegan)

# Módulos ---- 

#source('módulos/get_data.R')
source('módulos/get_data2.R')
source('módulos/CRE.R')
source('módulos/GST.R')

# UI ----
sb_ca <- function(nombre, id) { 
  menuItem(nombre, tabName = id, icon = icon('water'),
           menuSubItem('Cuadros de resumen', tabName = paste(id, 'cre', sep = '_')),
           menuSubItem('Gráficas', tabName = paste(id, 'gst', sep = '_')))
}

sb_menu <- dashboardSidebar(
  sidebarMenu(menuItem('Inicio', tabName = 'inicio', icon = icon('house')),
              sb_ca('Laguna de Cajititlán', 'caji'),
              sb_ca('Laguna de Zapotlán', 'zapo'),
              sb_ca('Río Santiago', 'santi'),
              sb_ca('Río Zula - Lerma', 'lerma'),
              sb_ca('Río Verde', 'verde'),
              menuItem('Acerca de', tabName = 'about', icon = icon('circle-question')),
              menuItem('Contacto', tabName = 'contacto', icon = icon('envelope'))))

sb_body <- dashboardBody(
  tabItems(
    tabItem(tabName = 'inicio', box(includeMarkdown('www/README.md'), width = 12)),
    tabItem(tabName = 'caji_cre', cre_UI(id = 'caji_cre', dataset = caji_amb_tidy)),
    tabItem(tabName = 'caji_gst', gst_UI(id = 'caji_gst', dataset = caji_amb_tidy)),
    tabItem(tabName = 'zapo_cre', cre_UI(id = 'zapo_cre', dataset = zapo_amb_tidy)),
    tabItem(tabName = 'zapo_gst', gst_UI(id = 'zapo_gst', dataset = zapo_amb_tidy)),
    tabItem(tabName = 'santi_cre', cre_UI(id = 'santi_cre', dataset = santi_amb_tidy)),
    tabItem(tabName = 'santi_gst', gst_UI(id = 'santi_gst', dataset = santi_amb_tidy)),
    tabItem(tabName = 'lerma_cre', cre_UI(id = 'lerma_cre', dataset = lerma_amb_tidy)),
    tabItem(tabName = 'lerma_gst', gst_UI(id = 'lerma_gst', dataset = lerma_amb_tidy)),
    tabItem(tabName = 'verde_cre', cre_UI(id = 'verde_cre', dataset = verde_amb_tidy)),
    tabItem(tabName = 'verde_gst', gst_UI(id = 'verde_gst', dataset = verde_amb_tidy)),
    tabItem(tabName = 'about', box(includeMarkdown('www/README.md'), width = 12)),
    tabItem(tabName = 'contacto', 'cristofer.camarena@alumnos.udg.mx')))

UI <- dashboardPage(
  dashboardHeader(titleWidth = 0),
  sb_menu,
  sb_body
)

# Server ----
server <- function(input, output, session) {
  cre_server(id = 'caji_cre', dataset = caji_amb_tidy)
  cre_server(id = 'lerma_cre', dataset = lerma_amb_tidy)
  cre_server(id = 'zapo_cre', dataset = zapo_amb_tidy)
  cre_server(id = 'verde_cre', dataset = verde_amb_tidy)
  cre_server(id = 'santi_cre', dataset = santi_amb_tidy)
  
  gst_server(id = 'caji_gst', dataset = caji_amb_tidy)
  gst_server(id = 'lerma_gst', dataset = lerma_amb_tidy)
  gst_server(id = 'zapo_gst', dataset = zapo_amb_tidy)
  gst_server(id = 'verde_gst', dataset = verde_amb_tidy)
  gst_server(id = 'santi_gst', dataset = santi_amb_tidy)
}

#ShinyApp ----

shinyApp(UI, server) 