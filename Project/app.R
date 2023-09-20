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
library(rsconnect)
library(openmeteo)
library(ggplot2)
library(plotly)

#https://seniordesign.shinyapps.io/shiny_dashboard/
ui <- fluidPage(
  # Changing Background Color
  tags$style(
    HTML("body {background-color: #F5F7F8; margin: 0;}"),  # Extend background to the edges
    HTML(".container-fluid {padding-left: 0; padding-right: 0;}")  # Remove padding for the container
  ),
  # Adding Built-In Theme
  theme = bslib::bs_theme(bootswatch = "yeti"),
  # Header of Dashboard
  tags$style(HTML("
    .custom-title-panel {
      height: 80px;
      background-color: #0b2341;
      color: #FF8200;
      margin: 10;
      padding: 10px;
      display: flex;
      justify-content: center;
      align-items: center;
      margin-bottom: 20px;
    }
  ")),
  div(
    class = "custom-title-panel",
    titlePanel(
      h1("West Tennessee Solar Farm", align = "center"),
      windowTitle = "West Tennessee Solar Farm Dashboard"
    ),
  ),
  # Main Content
  div(
    style = "display: flex; flex-direction: column; min-height: 100vh;",
    div(
      style = "flex-grow: 1;",
      fluidRow(
        column(
          width = 5,
          leafletOutput("map", width = "100%", height = "50vh"),
          tags$style(HTML("#weather_info table { margin-left: auto; margin-right: 0;
                          width: 100% !important;}")),
          div(
            h4("Tomorrow's Forecast", style = "text-align: center; font-weight: bold;"),
            tableOutput("weather_info")
          )
        ),
        column(
          width = 7,
          tabsetPanel(
            id = "Sensor Tabs",
            tabPanel("Live Data",
                     plotlyOutput("clickedSensorPlot"),
            ),
            tabPanel("Daily Data",
                     dateInput("Date", "Start Date:", value = "2023-09-08",
                               min = "2023-09-08"),
                     plotlyOutput("dailySensorIrradiancePlot"),
                     div(
                       actionButton("toggleDetrendedButton", "Toggle Detrended Data", style = "float: left;", class = "btn-default"),
                       downloadButton("downloadDailyData", "Download Daily Data", class = "btn-default", style = "float: right;"),
                     )
            ),
          ),
        ),
      )
    ),
    div(
      style = "background-color: #0b2341; color: #F5F7FA; text-align: center; padding: 10px; display: flex; flex-direction: row; justify-content: space-between; align-items: center;",
      # Left side with images
      div(
        img(
          src = "UT-System-Primary-Left-Align-RGB-Orange.png",
          height = "66px",
          width = "auto",
          style = "margin-right: 10px;"
        ),
        img(
          src = "cropped-UTRF-logo-w-dots.png",
          height = "40px",
          width = "auto",
          style = "margin-right: 10px;"
        ),
        img(
          src = "ut-martin-primary-align-left-151.png",
          height = "32px",
          width = "auto",
          style = "margin-right: 20px;"
        )
      ),
      # Right side with text
      div(
        h5("This project is supported by the University of Tennessee Research Foundation, the Department of Computer Science, and the Department of Mathematics and Statistics at the University of Tennessee at Martin.", style = "font-weight: bold; font-size: 12px;")
      )
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
  
  output$clickedSensorPlot <- renderPlotly({
    data <- csv_data()
    recent_doy <- max(data$DOY)
    if(!is.null(clicked_marker())) {
      marker_label <- clicked_marker()
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      if (column_name %in% colnames(data)) {
        sensorData <- data[data$DOY == recent_doy, column_name]
        
        p <- ggplot(data = data[data$DOY == recent_doy, ],
                    aes(x = MINUTE, y = .data[[column_name]])) +
          geom_line() +
          labs(x = "Time (Hours)",
               y = paste(gsub("Marker ", "", marker_label), "Irradiance (W/m²)"),
               title = paste(marker_label, "Plot"))
        p <- p + scale_x_continuous(
          breaks = seq(0, 1440, by = 60),
          labels = seq(0, 24, by = 1),
          limits = c(0, 1440)
        )
        p <- ggplotly(p)
        p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      }
      return(p)
    } else {
      sensorData <- data[data$DOY == recent_doy, "Sensor.1"]
      
      p <- ggplot(data = data[data$DOY == recent_doy, ], 
                  aes(x = MINUTE, y = Sensor.1)) +
        geom_line() + 
        labs(x = "Time (Hours)",
             y = "Sensor 1 Irradiance (W/m²)",
             title = "Sensor 1 Plot")
      p <- p + scale_x_continuous(
        breaks = seq(0, 1440, by = 60),
        labels = seq(0, 24, by = 1),
        limits = c(0, 1440)
      )
      p <- ggplotly(p)
      p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      return(p)
    }
  })
  
  detrended <- reactiveVal(FALSE)
  
  observeEvent(input$toggleDetrendedButton, {
    detrended(!detrended())
  })
  
  detrended_data <- reactive({
    data <- csv_data()
    if (detrended()) {
      data$DetrendedValue <- detrend(data$irradiance)  # Adjust the column name
      return(data)
    } else {
      return(NULL)
    }
  })
  
  output$dailySensorIrradiancePlot <- renderPlotly({
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
        
        if(detrended()) {
          # Detrend the data
          detrended_sensor <- diff(filtered_data[, column_name])
          
          p_data <- data.frame(MINUTE = filtered_data$MINUTE[-nrow(filtered_data)], DetrendedValue = detrended_sensor)
          
          # Create the plot with MINUTE on the X-axis and detrended sensor irradiance on the Y-axis
          p <- ggplot(data = p_data, aes(x = MINUTE, y = DetrendedValue)) +
            geom_line() +
            labs(
              title = paste(marker_label, "Detrended Irradiance", format(input$Date, "%m-%d-%Y")),
              x = "Time (Minutes)",
              y = paste(marker_label, "Detrended Irradiance (W/m²)")
            )
          p <- p + scale_x_continuous(
            breaks = seq(0, 1440, by = 60),
            labels = seq(0, 24, by = 1),
            limits = c(0, 1440)
          )
          p <- ggplotly(p)
          p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
          return(p)
        } else {
          p <- ggplot(data = filtered_data, aes(x = MINUTE, y = .data[[column_name]])) +
            geom_line() +
            labs(
              title = paste(marker_label, "Irradiance", format(input$Date, "%m-%d-%Y")),
              x = "Time (Hours)",
              y = paste(marker_label, "Irradiance (W/m²)")
            )
          p <- p + scale_x_continuous(
            breaks = seq(0, 1440, by = 60),
            labels = seq(0, 24, by = 1),
            limits = c(0, 1440)
          )
          p <- ggplotly(p)
          p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
          return(p)
        }
      }
    } else {
      marker_label <- "Sensor 1"
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      
      filtered_data <- data[data$DOY == difference, c("MINUTE", column_name)]
      
      if (detrended()) {
        if (detrended()) {
          # Detrend the data
          detrended_sensor <- diff(filtered_data[, column_name])
          
          p_data <- data.frame(MINUTE = filtered_data$MINUTE[-nrow(filtered_data)], DetrendedValue = detrended_sensor)
          
          # Create the plot with MINUTE on the X-axis and detrended sensor irradiance on the Y-axis
          p <- ggplot(data = p_data, aes(x = MINUTE, y = DetrendedValue)) +
            geom_line() +
            labs(
              title = paste(marker_label, "Detrended Irradiance", format(input$Date, "%m-%d-%Y")),
              x = "Time (Minutes)",
              y = paste(marker_label, "Detrended Irradiance (W/m²)")
            )
          p <- p + scale_x_continuous(
            breaks = seq(0, 1440, by = 60),
            labels = seq(0, 24, by = 1),
            limits = c(0, 1440)
          )
          p <- ggplotly(p)
          p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
          return(p)
        }
      } else {
        # Use trended data (no detrending)
        # Create the plot with MINUTE on the X-axis and sensor irradiance on the Y-axis
        p <- ggplot(data = filtered_data, aes(x = MINUTE, y = .data[[column_name]])) +
          geom_line() +
          labs(
            title = paste(marker_label, "Irradiance", format(input$Date, "%m-%d-%Y")),
            x = "Time (Hours)",
            y = paste(marker_label, "Irradiance (W/m²)")
          )
        p <- p + scale_x_continuous(
          breaks = seq(0, 1440, by = 60),
          labels = seq(0, 24, by = 1),
          limits = c(0, 1440)
        )
        p <- ggplotly(p)
        p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        return(p)
      }
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
