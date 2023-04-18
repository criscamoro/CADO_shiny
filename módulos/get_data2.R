# Obtener datos de repositorio Github
# Prueba local

# Argumentos para la función ----

nom <- c('caji', # Laguna de Cajititlán
         'zapo', # Laguna de Zapotlán
         'verde', # Río Verde
         'lerma', # Río Zula-Lerma
         'santi') # Río Santiago

# Función para leer datos
# 'readRDS' será sustituida por una función que permita leer directamente del repositorio

datos <- function(n) {
  assign(x = paste(n, '_amb_tidy', sep = ''), 
         value = readRDS(paste('C:/Users/ccopi/Desktop/PP/Caji/datos/tidy/', n, '_amb_tidy.rds', sep = '')),
         envir = .GlobalEnv)
}

lapply(nom, datos)