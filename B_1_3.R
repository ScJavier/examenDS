


library(sparklyr)
library(tidyverse)

sc <- spark_connect(master = "local", version = '2.1.0')



####################################
# Parte 1
####################################

precios <- spark_read_csv(sc, name = 'precios', path = 'all_data.csv', infer_schema = FALSE)

# Número de registros
precios %>%
  count


# Número de categorías
precios %>%
  select(categoria) %>%
  distinct %>%
  count


# Número de cadenas comerciales
precios %>%
  select(cadenaComercial) %>%
  distinct %>%
  count


# Productos más monitoreados por entidad

precios2 <- precios %>%
  filter(!(estado %in% c(" COL. EDUARDO GUERRA", " ESQ. SUR 125\"", "estado")))
  
prod_ent <-
  precios2 %>%
  select(estado, producto) %>%
  group_by(estado, producto) %>%
  summarise(n = n()) %>%
  mutate(r = rank(-n)) %>%
  arrange(r) %>%
  filter(r <= 10) %>%
  collect

prod_ent <- arrange(prod_ent, estado, r)
write.csv(prod_ent, '1d_prod_ent.csv')


# Cadenas comerciales con más productos
prod_cad <- 
  precios %>%
  select(cadenaComercial, producto) %>%
  group_by(cadenaComercial) %>%
  summarise(n = n()) %>%
  mutate(r = rank(-n)) %>%
  arrange(r) %>%
  filter(r <= 10) %>%
  collect
prod_cad

# Calidad de los datos



#############################
# Parte 3
#############################

leon <-
  precios %>%
  filter(estado == 'GUANAJUATO', municipio %in% c('LEON', 'LEÓN')) %>%

spark_write_csv(leon, './precios_leon.csv')



precios_leon <- spark_read_csv(sc, 'precios_leon', 'precios_leon')

datos <- precios_leon %>%
  filter(!is.na(categoria), !is.na(producto), !is.na(presentacion),
         !is.na(precio), !is.na(fechaRegistro),
         !is.na(latitud), !is.na(longitud)) %>%
  collect

library(lubridate)

datos <- datos %>%
  mutate(anioR = year(fechaRegistro),
         mesR = month(fechaRegistro),
         precio = as.numeric(precio),
         latitud = as.numeric(latitud),
         longitud = as.numeric(longitud))

datos <- filter(datos, !is.na(longitud))

resumen <- datos %>%
  group_by(latitud, longitud, cadenaComercial,
           categoria,
           anioR, mesR) %>%
  summarise(precio_mean = mean(precio))


write.csv(resumen, 'resumen_Leon.csv')
 
