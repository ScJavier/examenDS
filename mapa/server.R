library(shiny)
library(leaflet)
library(dplyr)

resumen <- read.csv('resumen_Leon.csv')

resumen <- resumen %>%
  mutate(fecha = paste0(anioR,'-',ifelse(mesR<=9, paste0('0', mesR), mesR)))


server <- function(input, output, session) {
  
  datos_app <- reactive({
    datos <- resumen %>%
      filter(categoria == input$slideCategoria,
             fecha == input$slideFecha)
    
    datos
  })
  
  
  output$mapaPrueba <- renderLeaflet({
    base <- datos_app()
    paleta <- colorNumeric('inferno', base$precio_mean)
    
    r_birthplace_map <- leaflet(base) %>%
      addTiles() %>%  # use the default base map which is OpenStreetMap tiles
      addCircles(lng=~longitud, lat=~latitud,
                 color = ~paleta(precio_mean),
                 fillColor = ~paleta(precio_mean),
                 fillOpacity = 1,
                 popup = ~paste0('Precio: ',
                                 as.character(round(precio_mean, 1)),
                                 ' MXN'),
                 radius = ~150) %>%
      addLegend(title = 'Precio promedio',
                position = 'topright',
                pal = paleta,
                values = ~precio_mean)
    r_birthplace_map
  })
  
}