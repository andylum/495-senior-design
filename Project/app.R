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
library(googledrive)

ui <- fluidPage(
  titlePanel(
    h1("West Tennessee Solar Farm Dashboard", align = "center")
  ),
  fluidRow(
    column(width = 5,  # Adjust the width as needed
           leafletOutput("map", width = "100%", height = "50vh"),
           sliderInput("Date", "Dates", step = 7, 
                       min = as.Date("01-01-2023", "%m-%d-%Y"), 
                       max = as.Date("12-12-2023", "%m-%d-%Y"), 
                       value = as.Date("01-01-2023", "%m-%d-%Y"))),
    column(width = 4, 
           plotOutput("sensorPlot")
    ),
  )
)

server <- function(input, output, session) {
  read_csv_data <- function() {
    csv_data <- read.csv("https://docs.google.com/spreadsheets/d/1wZxQe8Sj9FtN7munorspl45K-xAGlpwKOs3R7lvLqkg/gviz/tq?tqx=out:csv")
    return(csv_data)
  }
  
  csv_data <- reactivePoll(
    intervalMillis = 100,  # Update interval in milliseconds 
    session = session,
    checkFunc = function() {
      Sys.time()  # Always return current time to trigger updates
    },
    valueFunc = function() {
      read_csv_data()
    }
  )
  
  output$sensorTable <- renderTable({
    csv_data()
  })
  
  output$map <- renderLeaflet({
    leaflet(leafletOptions(maxZoom = 17, minZoom = 17)) %>%
      addTiles(options = tileOptions(minZoom = 16, maxZoom = 16)) %>%
      addProviderTiles(providers$USGS.USImagery) %>%
      setView(lng = -89.389436, lat = 35.409276, zoom = 16) %>%
      setMaxBounds(lng1 = -89.389436, lat1 = 35.409276, 
                   lng2 = -89.389436, lat2 = 35.40926) %>%
      addCircleMarkers(lng = -89.390113, lat = 35.407478, popup = "Marker 1") %>%
      addCircleMarkers(lng = -89.391825, lat = 35.408833, popup = "Marker 2") %>%
      addCircleMarkers(lng = -89.391935, lat = 35.409777, popup = "Marker 3") %>%
      addCircleMarkers(lng = -89.391956, lat = 35.410766, popup = "Marker 4") %>%
      addCircleMarkers(lng = -89.392733, lat = 35.409819, popup = "Marker 5") %>%
      addCircleMarkers(lng = -89.389521, lat = 35.411284, popup = "Marker 6") %>%
      addCircleMarkers(lng = -89.386922, lat = 35.409485, popup = "Marker 7") %>%
      addCircleMarkers(lng = -89.388013, lat = 35.408433, popup = "Marker 8")
  })
  
  output$sensorPlot <- renderPlot({
    data <- csv_data()
    sensorData <- data$`Sensor.1`
    plot(sensorData, type = "l", 
         xlab = "Time",
         ylab = "Sensor 1 Irradiance",
         main = "Sensor 1 Plot",
         xlim = c(1, 1440), ylim = c(-10, max(550, max(sensorData + 10))))
  })
}

shinyApp(ui, server)


