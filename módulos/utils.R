# Utilidades

#cargar data sets
caji.amb.tidy <- read_csv('datos/tidy/caji_amb_tidy.csv')
zapo.amb.tidy <- read_csv('datos/tidy/zapo_amb_tidy.csv')
lerma.amb.tidy <- read_csv('datos/tidy/lerma_amb_tidy.csv')
verde.amb.tidy <- read_csv('datos/tidy/verde_amb_tidy.csv')
santi.amb.tidy <- read_csv('datos/tidy/santi_amb_tidy.csv')

# Coeficiente de Variación
cv <- function(x) {
  c.v <- (sd(x, na.rm = T)/mean(x, na.rm = T)*100)
}

sb.ca <- function(nombre, id) { 
  menuItem(nombre, tabName = id, icon = icon('water'),
           menuSubItem('Cuadros de resumen', tabName = paste(id, '_cre', sep = '')),
           menuSubItem('Gráficas', tabName = paste(id, '_gst', sep = '')))
}

#Selector de datasets
id.data <- function(x) {
  if(x == 'caji') {
    return(caji.amb.tidy)
  }
  if(x == 'lerma') {
    return(lerma.amb.tidy)
  }
  if(x == 'zapo') {
    return(zapo.amb.tidy)
  }
  if(x == 'verde') {
    return(verde.amb.tidy)
  }
  if(x == 'santi') {
    return(santi.amb.tidy)
  }
}
