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
#library(rsconnect)
#source("config.R")
library(openmeteo)

#https://seniordesign.shinyapps.io/shiny_dashboard/
ui <- fluidPage(
  #Changing Background Color
  tags$style(
    HTML("body {background-color: #F5F7F8;}")
  ),
  #Adding Built-In Theme
  theme = bslib::bs_theme(bootswatch = "yeti"),
  #Header of Dashboard
  titlePanel(
    h1("West Tennessee Solar Farm", align = "center", 
       style = "background-color: #0b2341; color: #FF8200; 
       width: 100vw; margin: -20px 0 0; padding: 10px; margin-left: -15px; margin-bottom: 10px;"),
    windowTitle = "West Tennessee Solar Farm Dashboard"
  ),
  #Splits Window Into Columns
    fluidRow(
      column(width = 5,  # Adjust the width as needed
             leafletOutput("map", width = "100%", height = "50vh"),
             tags$style(HTML("#weather_info table { margin-left: auto; margin-right: 0;
                      width: 100% !important;}")),
             div(
             h4("Tomorrow's Forecast", style = "text-align: center; font-weight: bold;"),
             tableOutput("weather_info")
             )
             ),
      column(width = 7,
             tabsetPanel(
               id = "Sensor Tabs",
               tabPanel("Live Data",
                        plotOutput("clickedSensorPlot"),
                        ),
               tabPanel("Daily Data",
                        dateInput("Date", "Start Date:", value = "2023-09-08", 
                                  min = "2023-09-08"),
                        plotOutput("dailySensorIrradiancePlot"),
                        div(
                          downloadButton("downloadDailyData", "Download Daily Data", class = "btn-lg"),
                        )
                  ),
               ),
             ),
    ),
  div(
    style = "position: relative; width: 100vw; bottom: -50px; margin-left: -15px;",
    div(
      style = "position: absolute; bottom: -65px; left: 0; width: 100%; background-color: #0b2341; text-align: center; padding: 10px;",
      h5("This project is supported by the University of Tennessee Research Foundation, the Department of",
         style = "color: #F5F7FA; font-weight: bold; text-align: right; font-size: 11px;"),
      h5("Computer Science, and the Department of Mathematics and Statistics at the University of Tennessee at Martin.",
         style = "color: #F5F7FA; font-weight: bold; text-align: right; font-size: 11px;")
    ),
    # UT SYSTEM LOGO
    div(
      style = "position: absolute; bottom: -52px; left: 15px;", # Set bottom to 0 to position at the very bottom of the dashboard
      img(src = "UT-System-Primary-Left-Align-RGB-Orange.png", 
          height = "60px", width = "auto")
    ),
    # UT RESEARCH FOUNDATION LOGO
    div(
      style = "position: absolute; bottom: -47px; left: 210px;", # Adjust the left position as needed
      img(src = "cropped-UTRF-logo-w-dots.png", 
          height = "40px", width = "auto")
    ),
    # UTM LOGO
    div(
      style = "position: absolute; bottom: -37px; left: 415px; align-items: center;", # Adjust the left position as needed
      img(src = "ut-martin-primary-align-left-151.png",
          height = "30px", width = "auto")  # Adjust the width and height as needed
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
  
  customColorPalette <- colorRampPalette(c("red", "lightgreen", "green"))(1000)
  colorPalette <- colorNumeric(
    palette = customColorPalette,
    domain = markers_data$irradiance
  )
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
        popup = ~paste("Sensor: ", label, "<br>Current Irradiance: ", irradiance),
        color = ~colorPalette(markers_data$irradiance)
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
        popup = ~paste(label, "<br>Current Irradiance: ", irradiance),
        color = ~colorPalette(markers_data$irradiance)
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
    recent_doy <- max(data$DOY)
    if(!is.null(clicked_marker())) {
      marker_label <- clicked_marker()
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      if (column_name %in% colnames(data)) {
        sensorData <- data[data$DOY == recent_doy, column_name]
        plot(sensorData, type = "l",
             xlab = "Time (Minutes)",
             ylab = paste(gsub("Marker ", "", marker_label), "Irradiance (W/m²)"),
             main = paste(marker_label, "Plot"),
             xlim = c(1, 1440), ylim = c(-10, max(550, max(sensorData + 10))))
        grid()
      }
    } else {
      sensorData <- data[data$DOY == recent_doy, "Sensor.1"]
      plot(sensorData, type = "l",
           xlab = "Time (Minutes)",
           ylab = "Sensor 1 Irradiance (W/m²)",
           main = "Sensor 1 Plot",
           xlim = c(1, 1440), ylim = c(-10, max(550, max(sensorData + 10))))
      grid()
      }
  })
  
  output$dailySensorIrradiancePlot <- renderPlot({
    data <- csv_data()
    selected_date <- as.Date(input$Date)
    start_date <- as.Date("2023-09-08")
    difference <- as.integer(selected_date - start_date) + 1
    
    if (!is.null(clicked_marker())) {
      marker_label <- clicked_marker()
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      
      if (column_name %in% colnames(data)) {
        # Filter data for the selected DOY and sensor
        filtered_data <- data[data$DOY == difference, c("MINUTE", column_name)]
        
        # Create the plot with MINUTE on the X-axis and sensor irradiance on the Y-axis
        plot(filtered_data$MINUTE, filtered_data[, column_name], type = "l",
             xlab = "Time (Minutes)",
             ylab = paste(gsub("Marker ", "", marker_label), "Irradiance (W/m²)"),
             main = paste(marker_label, "Irradiance", format(input$Date, "%m-%d-%Y")),
             xlim = c(1, 1440), ylim = c(-10, max(550, max(filtered_data[, column_name] + 10))))
        grid()
      }
    } else {
      marker_label <- "Sensor 1"
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      
      # Filter data for the selected DOY and sensor
      filtered_data <- data[data$DOY == difference, c("MINUTE", column_name)]
      
      # Create the plot with MINUTE on the X-axis and sensor irradiance on the Y-axis
      plot(filtered_data$MINUTE, filtered_data[, column_name], type = "l",
           xlab = "Time (Minutes)",
           ylab = paste(gsub("Marker ", "", marker_label), "Irradiance (W/m²)"),
           main = paste(marker_label, "Irradiance", format(input$Date, "%m-%d-%Y")),
           xlim = c(1, 1440), ylim = c(-10, max(550, max(filtered_data[, column_name] + 10))))
      grid()
    }
  })
  
  output$downloadDailyData <- downloadHandler(
    filename = function() {
      selected_date <- as.Date(input$Date)
      file_name <- paste("Daily_Data_", format(selected_date, "%Y-%m-%d"), ".csv", sep = "")
      return(file_name)
    },
    content = function(file) {
      selected_date <- as.Date(input$Date)
      start_date <- as.Date("2023-09-08")
      difference <- as.integer(selected_date - start_date) + 1
      
      data <- csv_data()
      
      # Filter data for the selected DOY
      filtered_data <- data[data$DOY == difference, ]
      
      # Write the filtered data to the CSV file
      write.csv(filtered_data, file, row.names = FALSE)
    }
  )
  
  updateWeatherData <- reactivePoll(
    intervalMillis = 10000,
    session = session,
    checkFunc = function() {
      Sys.time()
    },
    valueFunc = function() {
      coords <- c(35.50,-89.40)

      start_date <- format(Sys.Date() + 1, format = "%Y-%m-%d")
      end_date <- format(Sys.Date() + 1, format = "%Y-%m-%d")

      weather_data <- weather_forecast(
        location = coords,
        start = start_date,
        end = end_date,
        hourly = c("cloudcover", "temperature_2m"),
        daily = c("weathercode", "shortwave_radiation_sum"),
        response_units = list(temperature_unit = "fahrenheit")
      )
      average_cloud_cover <- round(mean(weather_data$hourly_cloudcover, na.rm = TRUE), 2)
      average_temperature <- round(mean(weather_data$hourly_temperature_2m, na.rm = TRUE), 2)

      sky_weather <- head(weather_data$daily_weathercode, 1)
      uv_radiation <- head(weather_data$daily_shortwave_radiation_sum, 1)
      return(list(cloud_cover = average_cloud_cover, temperature = average_temperature,
                  code = sky_weather, uv = uv_radiation))
    }
  )

  output$weather_info <- renderTable({
    cloud_cover <- updateWeatherData()$cloud_cover
    temperature <- updateWeatherData()$temperature
    sky_weather <- updateWeatherData()$code
    uv <- updateWeatherData()$uv
    if(sky_weather == 0) {
      sky_weather <- "Clear Sky"
    } else if(sky_weather == 1 | sky_weather == 2) {
      sky_weather <- "Partly Cloudy"
    } else if(sky_weather == 3) {
      sky_weather <- "Overcast"
    } else if(sky_weather == 45 | sky_weather == 48) {
      sky_weather <- "Foggy"
    } else if((sky_weather >= 51 & sky_weather <= 57) ) {
      sky_weather <- "Drizzle"
    } else if(sky_weather == 61 | sky_weather == 80) {
      sky_weather <- "Light Rain"
    } else if(sky_weather == 63 | sky_weather == 65 | sky_weather == 81 | sky_weather == 82) {
      sky_weather <- "Moderate/Heavy Rain"
    } else if(sky_weather == 66 | sky_weather == 67) {
      sky_weather <- "Freezing Rain"
    } else if((sky_weather >= 71 & sky_weather <= 77) | sky_weather == 85 | sky_weather == 86) {
      sky_weather <- "Snow"
    } else {
      sky_weather <- "Thunderstorm"
    }
    weather_info <- data.frame(
        Date = format(Sys.Date() + 1, format = "%Y-%m-%d"),
        Metric = c("Predicted Average Cloud Cover (%)", "Predicted Average Temperature °F", "Weather Outlook", "Predicted UV Radiation"),
        Value = c(cloud_cover,temperature, sky_weather, uv)
    )

    return(weather_info)
  })
  session$onSessionEnded(stopApp)
}
shinyApp(ui, server)
