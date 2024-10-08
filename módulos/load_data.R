# Obtener datos del repositorio Github https://github.com/criscamoro/CADO

# Argumentos para la función ----
nom <- c(
  "caji", # Laguna de Cajititlán
  "zapo", # Laguna de Zapotlán
  "verde", # Río Verde
  "lerma", # Río Zula-Lerma
  "santi" # Río Santiago
) 

# Función para leer datos ----
datos <- function(n) {
  assign(
    paste(n, "_amb_tidy", sep = ""),
    read_csv(paste("https://raw.githubusercontent.com/criscamoro/CADO/main/datos/tidy/", n, "_amb_tidy.csv", sep = "")),
    .GlobalEnv
  )
}

lapply(nom, datos)

# Leer datos de fitoplancton ----
assign(
  "caji_fito_rect",
  read.csv("https://raw.githubusercontent.com/criscamoro/CADO/main/datos/rectangulares/caji_fito_rect.csv"),
  .GlobalEnv
)

assign(
  "caji_fito_tidy",
  read.csv("https://raw.githubusercontent.com/criscamoro/CADO/main/datos/tidy/caji_fito_tidy.csv"),
  .GlobalEnv
)