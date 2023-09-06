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
           leafletOutput("map", width = "100%", height = "50vh")
           ),
    column(width = 4, 
           plotOutput("clickedSensorPlot")
    )
  )
)

server <- function(input, output, session) {
  read_csv_data <- function() {
    csv_data <- read.csv("https://docs.google.com/spreadsheets/d/1wZxQe8Sj9FtN7munorspl45K-xAGlpwKOs3R7lvLqkg/gviz/tq?tqx=out:csv")
    return(csv_data)
  }
  
  markers_data <- data.frame(
    lat = c(35.407478, 35.408833, 35.409777, 35.410766, 35.409819, 35.411284, 35.409485, 35.408433),
    lng = c(-89.390113, -89.391825, -89.391935, -89.391956, -89.392733, -89.389521, -89.386922, -89.388013),
    label = c("Marker 1", "Marker 2", "Marker 3", "Marker 4", "Marker 5", "Marker 6", "Marker 7", "Marker 8")
  )
  
  csv_data <- reactivePoll(
    intervalMillis = 1000,  # Update interval in milliseconds 
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
      addCircleMarkers(
        data = markers_data,
        lat = ~lat,
        lng = ~lng,
        label = ~label,
        radius = 10
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
      column_name <- paste0("Sensor.", gsub("Marker ", "", marker_label))
      if (column_name %in% colnames(data)) {
        sensorData <- data[, column_name]
        plot(sensorData, type = "l",
             xlab = "Time",
             ylab = paste("Sensor", gsub("Marker ", "", marker_label), "Irradiance"),
             main = paste(marker_label, "Plot"),
             xlim = c(1, 1440), ylim = c(-10, max(550, max(sensorData + 10))))
      }
    }
  })
  
}

shinyApp(ui, server)


