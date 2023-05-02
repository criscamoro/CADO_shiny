# Aplicación de visualización de datos del Programa de Monitoreo de Cuerpos de Agua Dulce de Occidente (CAO)

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
library(treemapify)

# Módulos ----
source("módulos/load_data.R")
source("módulos/cuadros.R")
source("módulos/series_temp.R")
source("módulos/fito.R")

# UI ----
sb_ca <- function(nombre, id) {
  menuItem(nombre,
    tabName = id, icon = icon("water"),
    menuSubItem("Cuadros de resumen", tabName = paste(id, "_cre", sep = "")),
    menuSubItem("Gráficas", tabName = paste(id, "_gst", sep = ""))
  )
}

sb_menu <- dashboardSidebar(
  sidebarMenu(
    menuItem("Inicio", tabName = "inicio", icon = icon("house")),
    menuItem("Laguna de Cajititlán", 
      tabName = "caji", icon = icon("water"),
      menuSubItem("Cuadros de resumen", tabName = "caji_cre"),
      menuSubItem("Gráficas", tabName = "caji_gst"),
      menuSubItem("Fitoplancton", tabName = "caji_fito")),
    sb_ca("Laguna de Zapotlán", "zapo"),
    sb_ca("Río Verde", "verde"),
    sb_ca("Río Zula - Lerma", "lerma"),
    sb_ca("Río Santiago", "santi"),
    menuItem("Acerca de", tabName = "about", icon = icon("circle-question")),
    menuItem("Contacto", tabName = "contacto", icon = icon("envelope"))
  )
)

sb_body <- dashboardBody(
  tabItems(
    tabItem(tabName = "inicio", box(includeMarkdown("www/README.md"), width = 12)),
    tabItem(tabName = "caji_cre", cre_UI("caji_cre", caji_amb_tidy)),
    tabItem(tabName = "caji_gst", gst_UI("caji_gst", caji_amb_tidy)),
    tabItem(tabName = "caji_fito", fito_UI("caji_fito", caji_fito_rect)),
    tabItem(tabName = "zapo_cre", cre_UI("zapo_cre", zapo_amb_tidy)),
    tabItem(tabName = "zapo_gst", gst_UI("zapo_gst", zapo_amb_tidy)),
    tabItem(tabName = "verde_cre", cre_UI("verde_cre", verde_amb_tidy)),
    tabItem(tabName = "verde_gst", gst_UI("verde_gst", verde_amb_tidy)),
    tabItem(tabName = "lerma_cre", cre_UI("lerma_cre", lerma_amb_tidy)),
    tabItem(tabName = "lerma_gst", gst_UI("lerma_gst", lerma_amb_tidy)),
    tabItem(tabName = "santi_cre", cre_UI("santi_cre", santi_amb_tidy)),
    tabItem(tabName = "santi_gst", gst_UI("santi_gst", santi_amb_tidy)),
    tabItem(tabName = "about", "Versión 1.3 (02/05/2023)"),
    tabItem(tabName = "contacto", "cristofer.camarena@alumnos.udg.mx")
  )
)

UI <- dashboardPage(
  dashboardHeader(titleWidth = 0),
  sb_menu,
  sb_body
)

# Server ----
server <- function(input, output, session) {
  cre_server("caji_cre", caji_amb_tidy)
  cre_server("zapo_cre", zapo_amb_tidy)
  cre_server("verde_cre", verde_amb_tidy)
  cre_server("lerma_cre", lerma_amb_tidy)
  cre_server("santi_cre", santi_amb_tidy)

  gst_server("caji_gst", caji_amb_tidy)
  gst_server("zapo_gst", zapo_amb_tidy)
  gst_server("verde_gst", verde_amb_tidy)
  gst_server("lerma_gst", lerma_amb_tidy)
  gst_server("santi_gst", santi_amb_tidy)
  
  fito_server("caji_fito", caji_fito_rect)
}

# ShinyApp ----

shinyApp(UI, server)
