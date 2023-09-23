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
  # Main Content Split into Two Columns
  div(
    style = "display: flex; flex-direction: column; min-height: 89vh;",
    div(
      style = "flex-grow: 1;",
      # Contains map and forecast data
      fluidRow(
        column(
          width = 5,
          leafletOutput("map", width = "100%", height = "50vh"),
          # Makes Forecast Table align with map
          tags$style(HTML("#weather_info table { margin-left: auto; margin-right: 0;
                          width: 100% !important;}")),
          div(
            h4("Tomorrow's Forecast", style = "text-align: center; font-weight: bold;"),
            tableOutput("weather_info")
          )
        ),
        # Contains tabsetPanel. Tabs = Live Data, Daily Data
        column(
          width = 7,
          tabsetPanel(
            id = "Sensor Tabs",
            # Live Data Panel
            tabPanel("Live Data",
                     plotlyOutput("clickedSensorPlot"),
            ),
            # Daily Data Panel
            tabPanel("Daily Data",
                     # Allows User to pick which day's data to see
                     dateInput("Date", "Start Date:", value = "2023-09-08",
                               min = "2023-09-08"),
                     plotlyOutput("dailySensorIrradiancePlot"),
                     # Toggles between detrended and trended plots
                     div(
                       actionButton("toggleDetrendedButton", "Toggle Detrended Data", style = "float: left;", class = "btn-default"),
                       downloadButton("downloadDailyData", "Download Daily Data", class = "btn-default", style = "float: right;"),
                     )
            ),
          ),
        ),
      )
    ),
    # Footer Section
    div(
      style = "background-color: #0b2341; color: #F5F7FA; text-align: center; padding: 20px; display: flex; flex-direction: row; justify-content: space-between; align-items: center;",
      # Left side with images
      div(
        style = "display: flex; align-items: center;",
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
  # Reactively reads data
  read_csv_data <- function() {
    csv_data <- read.csv("https://docs.google.com/spreadsheets/d/1wZxQe8Sj9FtN7munorspl45K-xAGlpwKOs3R7lvLqkg/gviz/tq?tqx=out:csv", header = T)
    return(csv_data)
  }
  #Latitude, Longitude, and current Irradiance data for all sensors
  markers_data <- data.frame(
    lat = c(35.407478, 35.408833, 35.409777, 35.410766, 35.409819, 35.411284, 35.409485, 35.408433, 35.410268, 35.410074),
    lng = c(-89.390113, -89.391825, -89.391935, -89.391956, -89.392733, -89.389521, -89.386922, -89.388013, -89.386861, -89.385514),
    label = c("Sensor 1", "Sensor 2", "Sensor 3", "Sensor 4", "Sensor 5", "Sensor 6", "Sensor 7", "Sensor 8", "Sensor 9", "Sensor 10"),
    irradiance = rep(0, 10)
  )
  
  # Updates according to the update interval
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
  
  #If a sensor is low compared to the others, it will show up as more red. Otherwise it will look green.
  #Allows for easy visualization of downed sensors
  customColorPalette <- colorRampPalette(c("red", "lightgreen", "green"))(1000)
  colorPalette <- colorNumeric(
    palette = customColorPalette,
    domain = markers_data$irradiance
  )
  output$map <- renderLeaflet({
    leaflet(leafletOptions(maxZoom = 16, minZoom = 16)) %>%
      addTiles(options = tileOptions(minZoom = 16, maxZoom = 16)) %>%
      #Adds aerial view
      addProviderTiles(providers$USGS.USImagery) %>%
      #Centers view by latitude and longitude 
      setView(lng = -89.389436, lat = 35.409276, zoom = 16) %>%
      #Sets bounds of map by longitude and latitude
      setMaxBounds(lng1 = -89.389436, lat1 = 35.409276, 
                   lng2 = -89.389436, lat2 = 35.40926) %>%
      #Puts interactive markers on Leaflet Map using markers_data
      addCircleMarkers(
        data = markers_data,
        lat = ~lat,
        lng = ~lng,
        label = ~label,
        #Sets size of markers
        radius = 8,
        #Popups the current irradiance
        popup = ~paste("Sensor: ", label, "<br>Current Irradiance: ", irradiance),
        #Implements the color palette gradient
        color = ~colorPalette(markers_data$irradiance)
      )
  })
  
  #Allows the popups to reactively update
  observe({
    data <- csv_data()
    # Gets current irradiance data
    for (i in 1:10) {
      column_name <- paste0("Sensor.", i)
      if (column_name %in% colnames(data)) {
        markers_data$irradiance[i] <- tail(data[, column_name], 1)
      }
    }
    # Resets popups to reflect current irradiance
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
  #Initializes clicked marker value
  clicked_marker <- reactiveVal(NULL)
  
  #When clicking on a sensor, change clicked_marker to that sensor
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
  
  # Outputs plot based on clicked sensor in the live data tab
  output$clickedSensorPlot <- renderPlotly({
    #Gets data
    data <- csv_data()
    #Sets current day
    recent_doy <- max(data$DOY)
    #Checks to see if a marker was clicked
    if(!is.null(clicked_marker())) {
      marker_label <- clicked_marker()
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      #Tests to see if column data is available
      if (column_name %in% colnames(data)) {
        #Sets data to only the clicked sensor on the most recent day
        sensorData <- data[data$DOY == recent_doy, column_name]
        #Creates plot based on sensorData
        p <- ggplot(data = data[data$DOY == recent_doy, ],
                    aes(x = MINUTE, y = .data[[column_name]])) +
          geom_line() +
          labs(x = "Time (Hours)",
               y = paste(gsub("Marker ", "", marker_label), "Irradiance (W/m²)"),
               title = paste(marker_label, "Plot"))
        #Changes x scale
        p <- p + scale_x_continuous(
          breaks = seq(0, 1440, by = 60),
          labels = seq(0, 24, by = 1),
          limits = c(0, 1440)
        )
        #Changes y scale
        p <- p + scale_y_continuous(
          limits = c(-10, max(sensorData, 550)),
          breaks = seq(0, max(sensorData, 550), by = 100)
        )
        #Converts ggplot to plotly for better interactivity
        p <- ggplotly(p)
        #Disables zoom and drag features
        p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      }
      #Returns full plot
      return(p)
    #If no marker has been clicked, do this default behavior
    } else {
      #Sets data to most recent day for sensor 1
      sensorData <- data[data$DOY == recent_doy, "Sensor.1"]
      #Creates plot
      p <- ggplot(data = data[data$DOY == recent_doy, ], 
                  aes(x = MINUTE, y = Sensor.1)) +
        geom_line() + 
        labs(x = "Time (Hours)",
             y = "Sensor 1 Irradiance (W/m²)",
             title = "Sensor 1 Plot")
      #Changes x scale
      p <- p + scale_x_continuous(
        breaks = seq(0, 1440, by = 60),
        labels = seq(0, 24, by = 1),
        limits = c(0, 1440)
      )
      #Changes y scale
      p <- p + scale_y_continuous(
        limits = c(-10, max(sensorData, 550)),
        breaks = seq(0, max(sensorData, 550), by = 100)
      )
      #Converts ggplot to plotly for interactivity
      p <- ggplotly(p)
      #Disables zoom and drag
      p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
      return(p)
    }
  })
  
  #Initializes detrended button to regular data
  detrended <- reactiveVal(FALSE)
  
  #When the button is clicked, change states
  observeEvent(input$toggleDetrendedButton, {
    detrended(!detrended())
  })
  
  #If the button is in a detrended state, then output the detrended data
  detrended_data <- reactive({
    data <- csv_data()
    if (detrended()) {
      data$DetrendedValue <- detrend(data$irradiance)
      return(data)
    } else {
      return(NULL)
    }
  })
  
  #Renders plot for Daily Data value
  output$dailySensorIrradiancePlot <- renderPlotly({
    #Gets Data
    data <- csv_data()
    #Gets selected date for the daily plot
    selected_date <- as.Date(input$Date)
    #Sets start date for data collection
    start_date <- as.Date("2023-09-08")
    #Calculates difference between start date and selected data (DOY column)
    difference <- as.integer(selected_date - start_date) + 1
    
    #If a marker has been clicked
    if (!is.null(clicked_marker())) {
      #Gets Marker Data
      marker_label <- clicked_marker()
      #Converts from marker to Sensor.1 for data collection
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      
      #Checks to see if there is data in the column
      if (column_name %in% colnames(data)) {
        # Filters data for the selected DOY and sensor
        filtered_data <- data[data$DOY == difference, c("MINUTE", column_name)]
        if (nrow(filtered_data) == 0) {
          # Create a plot with a fixed y-axis scale from 0 to 550 and consistent x-axis limits and breaks
          p <- ggplot(data = NULL, aes(x = NULL, y = NULL)) +
            labs(
              title = paste(marker_label, "Irradiance", format(input$Date, "%m-%d-%Y")),
              x = "Time (Hours)",
              y = paste(marker_label, "Irradiance (W/m²)")
            ) +
            #Changes x axis scale
            scale_x_continuous(
              breaks = seq(0, 1440, by = 60),
              labels = seq(0, 24, by = 1),
              limits = c(0, 1440)
            ) +
            #Changes y axis scale
            scale_y_continuous(limits = c(0, 550), breaks = seq(0, 550, by = 100))
          #Converts ggplot to plotly for interactivity
          p <- ggplotly(p)
          #Disables zoom and drag
          p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
          return(p)
        }
        #Checks whether it is in a detrended state
        if(detrended()) {
          # Detrend the data
          detrended_sensor <- diff(filtered_data[, column_name])
          
          #Converts data to manipulable format
          p_data <- data.frame(MINUTE = filtered_data$MINUTE[-nrow(filtered_data)], DetrendedValue = detrended_sensor)
          
          # Create the plot with MINUTE on the X-axis and detrended sensor irradiance on the Y-axis
          p <- ggplot(data = p_data, aes(x = MINUTE, y = DetrendedValue)) +
            geom_line() +
            labs(
              title = paste(marker_label, "Detrended Irradiance", format(input$Date, "%m-%d-%Y")),
              x = "Time (Hours)",
              y = paste(marker_label, "Detrended Irradiance (W/m²)")
            )
          #Changes x scale
          p <- p + scale_x_continuous(
            breaks = seq(0, 1440, by = 60),
            labels = seq(0, 24, by = 1),
            limits = c(0, 1440)
          )
          #Converts ggplot to plotly for interactivity
          p <- ggplotly(p)
          #Disables zoom and drag
          p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
          return(p)
        #If not in detrended state, plot regular data
        } else {
          #Creates plot
          p <- ggplot(data = filtered_data, aes(x = MINUTE, y = .data[[column_name]])) +
            geom_line() +
            labs(
              title = paste(marker_label, "Irradiance", format(input$Date, "%m-%d-%Y")),
              x = "Time (Hours)",
              y = paste(marker_label, "Irradiance (W/m²)")
            )
          #Changes x scale
          p <- p + scale_x_continuous(
            breaks = seq(0, 1440, by = 60),
            labels = seq(0, 24, by = 1),
            limits = c(0, 1440)
          )
          #Changes y scale
          p <- p + scale_y_continuous(
            limits = c(-10, max(filtered_data[column_name], 550)),
            breaks = seq(0, max(filtered_data[column_name], 550), by = 100)
          )
          #Converts ggplot to plotly for interactivity
          p <- ggplotly(p)
          #Disables zoom and drag
          p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
          return(p)
        }
      }
    #If no marker has been clicked
    #Default behavior
    } else {
      marker_label <- "Sensor 1"
      column_name <- paste0("Sensor.", gsub("Sensor ", "", marker_label))
      
      #Gets data for selected day for sensor 1
      filtered_data <- data[data$DOY == difference, c("MINUTE", column_name)]
      
      #Checks that there is data for a row
      if (nrow(filtered_data) == 0) {
        # Create a plot with a fixed y-axis scale from 0 to 550 and consistent x-axis limits and breaks
        p <- ggplot(data = NULL, aes(x = NULL, y = NULL)) +
          labs(
            title = paste(marker_label, "Irradiance", format(input$Date, "%m-%d-%Y")),
            x = "Time (Hours)",
            y = paste(marker_label, "Irradiance (W/m²)")
          ) +
          #Changes x scale
          scale_x_continuous(
            breaks = seq(0, 1440, by = 60),
            labels = seq(0, 24, by = 1),
            limits = c(0, 1440)
          ) +
          #Changes y scale
          scale_y_continuous(limits = c(0, 550), breaks = seq(0, 550, by = 100))
        #Converts ggplot to plotly for interactivity
        p <- ggplotly(p)
        #Disables zoom and drag
        p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        return(p)
      }
      
      if (detrended()) {
        if (detrended()) {
          # Detrend the data
          detrended_sensor <- diff(filtered_data[, column_name])
          
          #Converts to manipulable data
          p_data <- data.frame(MINUTE = filtered_data$MINUTE[-nrow(filtered_data)], DetrendedValue = detrended_sensor)
          
          # Create the plot with MINUTE on the X-axis and detrended sensor irradiance on the Y-axis
          p <- ggplot(data = p_data, aes(x = MINUTE, y = DetrendedValue)) +
            geom_line() +
            labs(
              title = paste(marker_label, "Detrended Irradiance", format(input$Date, "%m-%d-%Y")),
              x = "Time (Hours)",
              y = paste(marker_label, "Detrended Irradiance (W/m²)")
            )
          #Changes x scale
          p <- p + scale_x_continuous(
            breaks = seq(0, 1440, by = 60),
            labels = seq(0, 24, by = 1),
            limits = c(0, 1440)
          )
          #Converts ggplot to plotly for interactivity
          p <- ggplotly(p)
          p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
          return(p)
        }
      #If button is not in detrended state
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
        #Changes x scale
        p <- p + scale_x_continuous(
          breaks = seq(0, 1440, by = 60),
          labels = seq(0, 24, by = 1),
          limits = c(0, 1440)
        )
        #Changes y scale
        p <- p + scale_y_continuous(
          limits = c(-10, max(filtered_data[column_name], 550)),
          breaks = seq(0, max(filtered_data[column_name], 550), by = 100)
        )
        #Converts ggplot to plotly for interactivity
        p <- ggplotly(p)
        #Disables zoom and drag
        p <- p %>% layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
        return(p)
      }
    }
  })
  
  #Allows the user to download all data for selected day
  output$downloadDailyData <- downloadHandler(
    #Sets default file name to Daily_Data_(selected day).csv
    filename = function() {
      selected_date <- as.Date(input$Date)
      file_name <- paste("Daily_Data_", format(selected_date, "%Y-%m-%d"), ".csv", sep = "")
      return(file_name)
    },
    #Gets all data for the selected day for the csv
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
  
  #Updates openmeteo weather API  
  updateWeatherData <- reactivePoll(
    intervalMillis = 10000, #Update interval in milliseconds
    session = session,
    checkFunc = function() {
      Sys.time()
    },
    #Calls API
    valueFunc = function() {
      #Closest openmeteo weather station to the solar farm
      coords <- c(35.50,-89.40)
      
      #Gets data for tomorrow
      start_date <- format(Sys.Date() + 1, format = "%Y-%m-%d")
      end_date <- format(Sys.Date() + 1, format = "%Y-%m-%d")
      
      #Calls Openmeteo Weather API
      weather_data <- weather_forecast(
        location = coords,
        start = start_date,
        end = end_date,
        #Gets cloud cover, temperature, qualitative weather, and uv radiation
        hourly = c("cloudcover", "temperature_2m"),
        daily = c("weathercode", "shortwave_radiation_sum"),
        response_units = list(temperature_unit = "fahrenheit")
      )
      #Averages hourly values
      average_cloud_cover <- round(mean(weather_data$hourly_cloudcover, na.rm = TRUE), 2)
      average_temperature <- round(mean(weather_data$hourly_temperature_2m, na.rm = TRUE), 2)

      sky_weather <- head(weather_data$daily_weathercode, 1)
      uv_radiation <- head(weather_data$daily_shortwave_radiation_sum, 1)
      
      #Returns all data from the API call
      return(list(cloud_cover = average_cloud_cover, temperature = average_temperature,
                  code = sky_weather, uv = uv_radiation))
    }
  )
  
  #Formats the data from the weather API into a user-friendly table
  output$weather_info <- renderTable({
    cloud_cover <- updateWeatherData()$cloud_cover
    temperature <- updateWeatherData()$temperature
    sky_weather <- updateWeatherData()$code
    uv <- updateWeatherData()$uv
    
    #Changes weathercode to User-Readable format
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
    #Creates table
    weather_info <- data.frame(
        Date = format(Sys.Date() + 1, format = "%Y-%m-%d"),
        Metric = c("Predicted Average Cloud Cover (%)", "Predicted Average Temperature °F", "Weather Outlook", "Predicted UV Radiation"),
        Value = c(cloud_cover,temperature, sky_weather, uv)
    )
    
    return(weather_info)
  })
  #When closing out of the browser, the app automatically stops
  session$onSessionEnded(stopApp)
}
#Run the app
shinyApp(ui, server)
