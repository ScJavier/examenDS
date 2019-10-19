
library(sparklyr)

sc <- spark_connect(master = 'local', version = '2.1.0')

precios <- spark_read_csv(sc, name = 'precios', path = 'all_data.csv', infer_schema = FALSE)



# Canasta básica
canasta <- precios %>%
  group_by(producto) %>%
  summarise(n = n()) %>%
  mutate(r = rank(-n)) %>%
  filter(r <= 10) %>%
  collect

write.csv(canasta, 'canasta.csv')

productos_canasta <- canasta$producto

precios_canasta <- precios %>%
  filter(producto %in% productos_canasta)

# precios_canasta %>% summarise(n = n())

precios_canasta2 <- precios_canasta %>%
  filter(!(estado %in% c(" COL. EDUARDO GUERRA", " ESQ. SUR 125\"", "estado")))


precios_canasta3 <- precios_canasta2 %>%
  mutate(municipio = regexp_replace(municipio, "Á", "A")) %>%
  mutate(municipio = regexp_replace(municipio, "É", "E")) %>%
  mutate(municipio = regexp_replace(municipio, "Í", "I")) %>%
  mutate(municipio = regexp_replace(municipio, "Ó", "O")) %>%
  mutate(municipio = regexp_replace(municipio, "Ú", "U")) %>%
  mutate(municipio = regexp_replace(municipio, '\"', '')) %>%
  mutate(municipio = trim(municipio)) %>%
  mutate(municipio = regexp_replace(municipio, "CUAUTITLAN IZCALLI", "CUAUTITLAN")) %>%
  mutate(municipio = regexp_replace(municipio, "CUAUTITLAN", "CUAUTITLAN IZCALLI")) %>%
  mutate(municipio = regexp_replace(municipio, "ATIZAPAN DE ZARAGOZA", "ATIZAPAN")) %>%
  mutate(municipio = regexp_replace(municipio, "ATIZAPAN", "ATIZAPAN DE ZARAGOZA"))
  
municipios <- precios_canasta3 %>% select(municipio) %>% distinct %>% collect
municipios




costo_ciudades <- precios_canasta3 %>%
  group_by(estado, municipio, producto) %>%
  summarise(precio_m = mean(as.numeric(precio), na.rm = T)) %>%
  group_by(estado, municipio) %>%
  summarise(canasta_m = mean(precio_m), n = n()) %>%
  arrange(n, desc(canasta_m)) %>%
  collect

write.csv(costo_ciudades, 'costo_ciudades.csv')


precios_patron <- precios_canasta3 %>%
  filter(municipio %in% c('LA PAZ', 'CHIAUTEMPAN')) %>%
  collect

precios_patron2 <- precios_patron %>%
  mutate(fecha = str_sub(fechaRegistro, 1, 10),
         fecha = ymd(fecha),
         trimestre = quarter(fecha, with_year = TRUE))

precios_patron3 <- precios_patron2 %>%
group_by(estado, municipio, producto, trimestre) %>%
  summarise(precio_m = mean(as.numeric(precio), na.rm = T)) %>%
  group_by(estado, municipio, trimestre) %>%
  summarise(canasta_m = mean(precio_m))

write.csv2(precios_patron3, 'ciudades_patron.csv')



precios_estados <- precios_canasta3 %>%
  group_by(estado, producto) %>%
  summarise(precio_m = mean(as.numeric(precio), na.rm = T)) %>%
  group_by(estado) %>%
  summarise(canasta_m = mean(precio_m)) %>%
  arrange(desc(canasta_m)) %>%
  collect

write.csv(precios_estados, 'precios_estados.csv')


precios_BCS <- precios_canasta3 %>%
  filter(estado == 'BAJA CALIFORNIA SUR') %>%
collect

precios_BCS2 <- precios_BCS %>%
  mutate(fecha = str_sub(fechaRegistro, 1, 10),
         fecha = ymd(fecha),
         anio = year(fecha),
         mes = month(fecha)) %>%
  group_by(anio, mes, producto) %>%
  summarise(precio_m = mean(as.numeric(precio), na.rm = T)) %>%
  group_by(anio, mes) %>%
  summarise(canasta_m = mean(precio_m)) %>%
  arrange(anio, mes)

precios_BCS2

write.csv(precios_BCS2, 'precios_BCS.csv')



 

            
            


