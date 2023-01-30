#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(lubridate)
library(sp)
library(sf)

bdat <- read.csv(file.path("data", "location_estimates_final.csv"))
#bdat <- read.csv(file.path( "location_estimates_final.csv"))

# filter data to remove NA values
bdat <- bdat %>%
  select(c(location.long, location.lat, arrive.date, depart.date, animal.id, deploy_id))

bdat <- bdat[complete.cases(bdat), ]

# calculate time differences
bdat_sp <- bdat %>%
  mutate(arrive = ymd(arrive.date),
         depart = ymd(depart.date)) %>%
  mutate(year = year(arrive)) %>%
  mutate(arr_month = month(arrive),
         dep_month = month(depart)) %>%
  mutate(dur = depart -arrive) %>%
  mutate(lat = location.lat, 
         lng = location.long)

#month_col = sort(unique(bdat_sp$arr_month))
palette1 <- colorNumeric(palette = 'viridis', unique(bdat_sp$arr_month), reverse = TRUE)

ui <- fluidPage(
  leafletOutput("map", width="80%", height="800px"),
   absolutePanel(top = 10, right = 10,
                selectInput("animal", "BirdTag",
                            unique(bdat_sp$animal.id)
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  # # Reactive expression for the data subsetted to what the user selected
  #tag = "rekn_arjut_full"
   filteredData <- reactive({
     bdat_sp[bdat_sp$animal.id == input$animal.id]
    # bdat_sp[bdat_sp$tag == tag,]
   })

  output$map <- renderLeaflet({
    leaflet() %>%
      #addTiles() %>%
      addProviderTiles("CartoDB.DarkMatter") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 2) %>%
      addMarkers(data = filteredData())
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # # should be managed in its own observer.
   observe({
    
    # leafletProxy("map", data = filteredData()) %>%
    #   clearShapes() %>%
    #   #addCircles(radius = ~dur, weight = 1, color = "#777777")
    #           #  fillColor = ~pal(palette1), fillOpacity = 0.7, popup = ~paste(tag)
    #   addCircleMarkers(
    #                weight = 2, color = ~palette1(bdat_sp$arr_month),
    #                #radius = ~dur/10,
    #                fillColor = ~palette1(bdat_sp$arr_month))

    #               addPolylines(data = bdat_sp1, lng = bdat_sp1$lon, lat = bdat_sp1$lat,
                 #               color = "white",  opacity = 0.1, stroke = TRUE) %>%

     #)
   })
  # 
  # # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = filteredData())
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- palette1
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = palette1, values = ~tag
  #     )
#    }
#  })
}

shinyApp(ui, server)
