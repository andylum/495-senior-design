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
  theme = bslib::bs_theme(bootswatch = "flatly"),
  titlePanel(
    h1("West Tennessee Solar Farm Dashboard", align = "center"),
    windowTitle = "West Tennessee Solar Farm Dashboard"
  ),
  fluidRow(
    column(width = 5,  # Adjust the width as needed
           leafletOutput("map", width = "100%", height = "50vh")
           ),
    column(width = 5, 
           tabsetPanel(
             id = "Sensor Tabs",
             tabPanel("Live Data",
                      plotOutput("clickedSensorPlot")
                      ),
             tabPanel("Daily Data")
           ),
    )
  )
)

server <- function(input, output, session) {
  read_csv_data <- function() {
    csv_data <- read.csv("https://docs.google.com/spreadsheets/d/1wZxQe8Sj9FtN7munorspl45K-xAGlpwKOs3R7lvLqkg/gviz/tq?tqx=out:csv", header = T)
    return(csv_data)
  }
  markers_data <- data.frame(
    lat = c(35.407478, 35.408833, 35.409777, 35.410766, 35.409819, 35.411284, 35.409485, 35.408433, 35.410268, 35.410074),
    lng = c(-89.390113, -89.391825, -89.391935, -89.391956, -89.392733, -89.389521, -89.386922, -89.388013, -89.386861, -89.385514),
    label = c("Sensor 1", "Sensor 2", "Sensor 3", "Sensor 4", "Sensor 5", "Sensor 6", "Sensor 7", "Sensor 8", "Sensor 9", "Sensor 10"),
    irradiance = rep(0, 10)
  )
  
  csv_data <- reactivePoll(
    intervalMillis = 10000,  # Update interval in milliseconds 
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
    leaflet(leafletOptions(maxZoom = 16, minZoom = 16)) %>%
      addTiles(options = tileOptions(minZoom = 16, maxZoom = 16)) %>%
      addProviderTiles(providers$USGS.USImagery) %>%
      setView(lng = -89.389436, lat = 35.409276, zoom = 16) %>%
      setMaxBounds(lng1 = -89.389436, lat1 = 35.409276, 
                   lng2 = -89.389436, lat2 = 35.40926) %>%
      addCircleMarkers(
        data = markers_data,
        lat = ~lat,
        lng = ~lng,
        label = ~label,
        radius = 8,
        popup = ~paste("Sensor: ", label, "<br>Current Irradiance: ", irradiance)
      )
  })
  
  observe({
    data <- csv_data()
    for (i in 1:10) {
      column_name <- paste0("Sensor.", i)
      if (column_name %in% colnames(data)) {
        markers_data$irradiance[i] <- tail(data[, column_name], 1)
      }
    }
    leafletProxy("map") %>%
      clearPopups() %>%
      addCircleMarkers(
        data = markers_data,
        lat = ~lat,
        lng = ~lng,
        label = ~label,
        radius = 8,
        popup = ~paste("Sensor: ", label, "<br>Current Irradiance: ", irradiance)
      )
  })
  
  clicked_marker <- reactiveVal(NULL)

  observeEvent(input$map_marker_click, {
    click_info <- input$map_marker_click
    marker_index <- which(
      markers_data$lat == click_info$lat & markers_data$lng == click_info$lng
    )
    if (length(marker_index) == 1) {
      clicked_marker(markers_data$label[marker_index])
    } else {
      clicked_marker(NULL)
    }
  }, ignoreInit = TRUE)
  
  output$clickedSensorPlot <- renderPlot({
    data <- csv_data()
    if(!is.null(clicked_marker())) {
      marker_label <- clicked_marker()
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      if (column_name %in% colnames(data)) {
        sensorData <- data[, column_name]
        plot(sensorData, type = "l",
             xlab = "Time",
             ylab = paste(gsub("Marker ", "", marker_label), "Irradiance"),
             main = paste(marker_label, "Plot"),
             xlim = c(1, 1440), ylim = c(-10, max(550, max(sensorData + 10))))
      }
    } else {
      sensorData <- data[, "Sensor.1"]
      plot(sensorData, type = "l",
           xlab = "Time",
           ylab = "Marker 1 Irradiance",
           main = "Marker 1 Plot",
           xlim = c(1, 1440), ylim = c(-10, max(550, max(sensorData + 10))))
      }
  })
  
}

shinyApp(ui, server)


