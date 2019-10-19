library(shiny)
library(leaflet)
library(dplyr)

resumen <- read.csv('resumen_Leon.csv')

resumen <- resumen %>%
  mutate(fecha = paste0(anioR,'-',ifelse(mesR<=9, paste0('0', mesR), mesR)))

ui <- fluidPage(
  titlePanel('Consulta precios: León, Guanajuato'),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = 'slideCategoria',
                  label = 'Categoria',
                  choices = unique(resumen$categoria)),
      selectInput(input = 'slideFecha',
                  label = 'Fecha (Anio-Mes)',
                  choices = sort(unique(resumen$fecha)))
      
    ),
    
    mainPanel(
      leafletOutput('mapaPrueba')
      
    )
  )
)
