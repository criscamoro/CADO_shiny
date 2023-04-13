# Función para cargar datos
library(tidyverse)
library(openxlsx)
library(qdap)
library(lubridate)

# Descargar datos directo de la CEA Jalisco #### OBSOLETO
caji <- c('https://www.ceajalisco.gob.mx/contenido/datos_abiertos/LagunaCajititlan.xlsx', 'Laguna de Cajititlán')
lerma <- c('https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioZula-Lerma.xlsx', 'Río Zula-Lerma')
zapo <- c('https://www.ceajalisco.gob.mx/contenido/datos_abiertos/LagunaZapotlan.xlsx', 'Laguna Zapotlán')
verde <- c('https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioVerde.xlsx', 'Río Verde')
santi <- c('https://www.ceajalisco.gob.mx/contenido/datos_abiertos/RioSantiago.xlsx', 'Río Santiago')

url <- tibble(caji, lerma, zapo, verde, santi)

desc.datos <- function(x){
  download.file(x, destfile = paste('datos/crudos/',last(last(str_split(x, '/'))), sep = ''), 
                pathquiet = T, mode = 'wb')
}

if (difftime(Sys.time(), file.info('datos/crudos/LagunaCajititlan.xlsx')$ctime) > 7) {
  lapply(url, desc.datos)
}

# Estructurar datos en formato tidy
# Función para acceder a hojas dentro del excel
xl.sheet <- function(x,i){
  sheet <- read_excel(paste('datos/crudos/',last(last(str_split(x, '/'))), sep = ''), 
             sheet = nth(excel_sheets((paste('datos/crudos/',last(last(str_split(x, '/'))), sep = ''))), -i))
}

xl.import <- function(x) {
  sheet <- read.xlsx(x[1], sheet = x[2])
}

# Función para procesar los datos
data.process <- function(j) {
  datos.amb <- xl.sheet(j, 3) %>% 
    filter(!is.na(fecha)) %>%
    filter(!between(idMuestra, 112361, 112610)) %>% 
    select(-1) %>% 
    mutate(valor = as.character(gsub('<', '', valor))) %>% 
    mutate(valor = as.numeric(gsub('-', '', valor))) %>%
    mutate(idParametro = as.character(idParametro)) %>% 
    mutate(idParametro = as.character(mgsub(xl.sheet(j, 2)$idParametros, xl.sheet(j, 2)$param, idParametro))) %>% 
    mutate(idPuntoMuestreo = as.character(mgsub(xl.sheet(j, 1)$idPunto, xl.sheet(j, 1)$clave, idPuntoMuestreo))) %>%
    pivot_wider(names_from = 'idParametro', values_from = 'valor') %>% 
    mutate(año = as.factor(year(fecha))) %>% 
    mutate(mes = as.factor(month(fecha))) %>% 
    rename(est = idPuntoMuestreo) %>% 
    mutate(est = as.factor(est)) %>% 
    relocate(est, .after = mes) %>% 
    relocate(fecha, .before = año)
}

# Datos de la Laguna de Cajititlán (Requieren de pasos extra de limpieza)
caji.amb.rect <- xl.sheet(caji, 3) %>%
  select(-1) %>% # quitar columna de idMuestra
  slice(-n()) %>% 
  slice(-(22155:22404)) %>% # contiene fechas erróneas, datos duplicados y estaciones sin observaciones
  mutate(fecha = as.Date(as.character(gsub('2017-04-24', '2017-04-27', fecha)))) %>% # fecha incorrecta
  mutate(valor = as.character(gsub('<', '', valor))) %>% 
  mutate(valor = as.numeric(gsub('-', '', valor))) %>% # "-" son valores NA
  mutate(idParametro = as.character(idParametro)) %>%
  mutate(idParametro = as.character(mgsub(xl.sheet(caji, 2)$idParametros, xl.sheet(caji, 2)$param, idParametro))) %>%
  mutate(idPuntoMuestreo = as.character(mgsub(slice(xl.sheet(caji, 1), -15)$idPunto, slice(xl.sheet(caji, 1), -15)$clave, idPuntoMuestreo)))

caji.amb.tidy <<- caji.amb.rect %>%
  pivot_wider(names_from = 'idParametro', values_from = 'valor') %>%
  mutate(año = as.factor(year(fecha))) %>% 
  mutate(mes = as.factor(month(fecha))) %>% 
  rename(est = idPuntoMuestreo) %>% 
  mutate(est = as.factor(est)) %>% 
  relocate(est, .after = mes) %>% 
  relocate(fecha, .before = año)

csv <- function(x, i) {
  write.csv(data.process(x), paste('datos/tidy/', i, '_amb_tidy.csv', sep = ''), row.names = F, na = '')
}



# lapply(csv, url)
