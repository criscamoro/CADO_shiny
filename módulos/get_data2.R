# Obtener datos de repositorio Github
# Prueba local

# Argumentos para la función ----

nom <- c(
  "caji", # Laguna de Cajititlán
  "zapo", # Laguna de Zapotlán
  "verde", # Río Verde
  "lerma", # Río Zula-Lerma
  "santi" # Río Santiago
) 

# Función para leer datos
# 'readRDS' será sustituida por una función que permita leer directamente del repositorio

datos <- function(n) {
  assign(
    paste(n, "_amb_tidy", sep = ""),
    readRDS(paste("C:/Users/ccopi/Desktop/PP/Caji/datos/tidy/", n, "_amb_tidy.rds", sep = "")),
    .GlobalEnv
  )
}

lapply(nom, datos)
