# Load necessary libraries
library(shiny)
library(leaflet)
library(googledrive)
library(rsconnect)
library(openmeteo)
library(ggplot2)
library(plotly)

# Define UI for the Shiny app
ui <- navbarPage(
  "Solar Farm Dashboard",
  tabPanel("Dashboard", 
           fluidPage(
             tags$style(
               HTML("body {background-color: #F5F7F8; margin: 0; padding: 0;}"),
               HTML(".container-fluid {padding-left: 0; padding-right: 0;}"),
             ),
             theme = bslib::bs_theme(bootswatch = "yeti"),
             tags$style(HTML("
        .custom-title-panel {
          height: 80px;
          background-color: #0b2341;
          color: #FF8200;
          margin: 0px;
          padding: 10px;
          display: flex;
          justify-content: center;
          align-items: center;
          margin-bottom: 20px;
          margin-top: -20px;
        }
        
      ")),
             div(
               class = "custom-title-panel",
               titlePanel(
                 h1("West Tennessee Solar Farm", align = "center"),
                 windowTitle = "West Tennessee Solar Farm Dashboard"
               ),
             ),
             div(
               style = "display: flex; flex-direction: column; min-height: 89vh;",
               div(
                 style = "flex-grow: 1;",
                 fluidRow(
                   column(
                     width = 5,
                     leafletOutput("map", width = "100%", height = "50vh"),
                     tags$style(HTML("#weather_info table { margin-left: auto; margin-right: 0; width: 100% !important;}")),
                     div(
                       h4("Tomorrow's Forecast", style = "text-align: center; font-weight: bold;"),
                       tableOutput("weather_info")
                     ),
                       div(
                         style = "background-color: white; padding: 10px; border-radius: 15px; width: 100%; text-align: center; box-shadow: 0 8px 12px rgba(0, 0, 0, 0.8); color: black; margin-top: 0px;",
                         HTML("<b>Irradiance</b> is a measure of how much energy from sunlight or other forms of light falls on a given area, typically expressed per square meter. It helps us understand how much light energy is reaching a specific spot, such as a solar panel.")
                       )
                   ),
                   
                   column(
                     width = 7,
                     style = "border: none;",
                     tabsetPanel(
                       id = "Sensor Tabs",
                       tabPanel("Live Data", plotlyOutput("clickedSensorPlot"),
                                style = "margin-top: -10px;"),
                       tabPanel("Daily Data",
                                dateInput("Date", "Start Date:", value = "2023-09-08", min = "2023-09-08"),
                                plotlyOutput("dailySensorIrradiancePlot"),
                                fluidRow(
                                  div(
                                    actionButton("togglePowerProductionButton", "Irradiance", style = "float: left;", class = "btn-default"),
                                    downloadButton("downloadDailyData", "Download Daily Data", class = "btn-default", style = "float: right;")
                                  )
                                )
                       ),
                     ),
                     div(
                       style = "background-color: white; padding: 10px; border-radius: 15px; text-align: left; box-shadow: 0 8px 12px rgba(0, 0, 0, 0.8); color: black; margin: 20px; height: auto; width: auto; align-items: center;",
                       uiOutput("total_production_text"),
                       img(
                         src = "front-page-illo-02-e1528216037613.png",
                         height = "auto",
                         width = "100%"
                       )
                     )
                   ),
                 ),
               ),
               div(
                 style = "background-color: #0b2341; color: #F5F7FA; text-align: center; padding: 20px; display: flex; flex-direction: row; justify-content: space-between; align-items: center;",
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
                 div(
                   h5("This project is supported by the University of Tennessee Research Foundation, the Department of Computer Science, and the Department of Mathematics and Statistics at the University of Tennessee at Martin.", style = "font-weight: bold; font-size: 12px;")
                 )
               )
             )
           )
  ),
  tabPanel("Solar Power Process", 
           tags$head(
             tags$meta(charset = "UTF-8"),
             tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
             tags$title("West Tennessee Solar Farm Process"),
             tags$style(HTML("
         /* Reset some default styles to make the page look cleaner */
        body, h1, p, ul, li {
            margin: 0px;
            padding: 0px;
        }

        body {
            font-family: \"Asap\", sans-serif;
            background-color: #00205B; /* UT Martin Navy Blue */
            color: #FF671F; /* UT Martin Orange */
            margin: 0px;
            padding: 0px;
        }

        section {
          min-height: 100vh;
          display: flex;
          justify-content: center;
          align-items: center;
          background: #00205B; /* UT Martin Navy Blue */
          color: #FF671F; /* UT Martin Orange */
          margin: 0px;
          padding: 0px;
        }


        section:nth-child(1) {
            color: #FF671F; /* UT Martin Orange */
            background: #00205B;
            margin: -20px;
            padding: 0px;
            
        }

        section:nth-child(2) {
            background: #FF671F; /* UT Martin Orange */
            color: #00205B; /* UT Martin Navy Blue */
            margin: -20px;
            padding: 0px;
        }

        section:nth-child(3) {
            color: #FF671F; /* UT Martin Orange */
            background: #00205B;
            margin: -20px;
            padding: 0px;
        }

        section:nth-child(4) {
            background: #FF671F; /* UT Martin Orange */
            color: #00205B; /* UT Martin Navy Blue */
            margin: -20px;
            padding: 0px;
        }

        section .container {
            margin: 0px;
            padding: 0px;
        }

        section h1 {
            font-size: 3rem;
            margin: -20px;
            padding: 0px;
        }

        section h2 {
            font-size: 40px;
            text-align: center;
            text-transform: uppercase;
            margin: 0px;
            padding: 0px;
        }

        section .text-container {
            display: flex;
            flex-wrap: wrap;
            justify-content: space-between;
            margin: 0px;
            padding: 0px;
        }

        section .text-container .text-box {
            flex-basis: calc(40% - 20px);
            margin: 0px;
            padding: 0px;
            color: #00205B; /* UT Martin Navy Blue */
        }

        section .text-container .text-box h3 {
            font-size: 30px;
            text-align: center;
            text-transform: uppercase;
            margin: 0px;
            padding: 0px;
        }

        @media (min-width: 100vh;) {
            section h1 {
                font-size: 2rem;
                text-align: center;
                margin: 0px;
                padding: 0px;
            }

            section .text-container .text-box {
                flex-basis: calc(100% - 40px);
                margin: 0px;
                padding: 0px;
            }
        }

        /* Style for one text box on the left and one on the right */
        section .text-container .text-box:nth-child(odd) {
            order: 1;
            margin: 0px;
            padding: 0px;
            color: #FFFFFF;
        }

        section .text-container .text-box:nth-child(even) {
            order: 2;
            margin: 0px;
            padding: 0px;
            color: #FFFFFF;
        }

        .reveal {
            position: relative;
            transform: translateY(100px);
            opacity: 0;
            transition: 1s all ease;
        }

        .reveal.active {
            transform: translateY(0);
            opacity: 1;
        }
    ")),
             tags$script(HTML("
        function reveal() {
            var reveals = document.querySelectorAll(\".reveal\");

            for (var i = 0; i < reveals.length; i++) {
                var windowHeight = window.innerHeight;
                var elementTop = reveals[i].getBoundingClientRect().top;
                var elementVisible = 150;

                if (elementTop < windowHeight - elementVisible) {
                    reveals[i].classList.add(\"active\");
                } else {
                    reveals[i].classList.remove(\"active\");
                }
            }
        }

        window.addEventListener(\"scroll\", reveal);
    "))
           ),
           tags$body(
             tags$section(
               tags$h1("Scroll Down to Explore How Solar Energy Works ↓")
             ),
             tags$section(
               class = "reveal",
               tags$h2("Step 1: Sunlight to Solar Panels", style = "margin-right: 20px;"),
               tags$div(
                 class = "text-container",
                 tags$div(
                   class = "text-box",
                   tags$h3("Sunlight Energy"),
                   tags$p("The sun sends powerful sunlight to our solar panels, providing energy.")
                 ),
                 tags$div(
                   class = "text-box",
                   tags$h3("Solar Panel Collection"),
                   tags$p("Solar panels collect this energy and convert it into electricity, like a magic power generator!")
                 )
               )
             ),
             tags$section(
               class = "reveal",
               tags$h2("Step 2: Photovoltaic Cells", style = "margin-right: 20px;"),
               tags$div(
                 class = "text-container",
                 tags$div(
                   class = "text-box",
                   tags$h3("Solar Cells"),
                   tags$p("Inside the solar panels, there are tiny cells called photovoltaic cells. They convert sunlight into electricity.")
                 ),
                 tags$div(
                   class = "text-box",
                   tags$h3("How It Works"),
                   tags$p("These cells use their superpowers to turn sunlight photons into electrical current that we can use.")
                 )
               )
             ),
             tags$section(
               class = "reveal",
               tags$h2("Step 3: Inverter Transformation", style = "margin-right: 20px;"),
               tags$div(
                 class = "text-container",
                 tags$div(
                   class = "text-box",
                   tags$h3("Inverter Function"),
                   tags$p("We use a special machine called an inverter to change the electricity from the panels into the type we use in our homes.")
                 ),
                 tags$div(
                   class = "text-box",
                   tags$h3("Conversion Process"),
                   tags$p("The inverter acts like a translator, converting the solar energy into regular electricity for our appliances.")
                 )
               )
             ),
             tags$section(
               class = "reveal",
               tags$h2("Step 4: Power for You", style = "margin-right: 20px;"),
               tags$div(
                 class = "text-container",
                 tags$div(
                   class = "text-box",
                   tags$h3("Grid Connection"),
                   tags$p("This electricity goes into a big power grid, and from there, it comes to your home to power your lights, TV, and more!")
                 ),
                 tags$div(
                   class = "text-box",
                   tags$h3("Your Home's Energy"),
                   tags$p("Once the power reaches your house, it's ready to make your life brighter and more convenient.")
                 )
               )
             )
           ),
           div(
             style = "background-color: #0b2341; color: #F5F7FA; text-align: center; padding: 20px; display: flex; flex-direction: row; justify-content: space-between; align-items: center;",
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
             div(
               h5("This project is supported by the University of Tennessee Research Foundation, the Department of Computer Science, and the Department of Mathematics and Statistics at the University of Tennessee at Martin.", style = "font-weight: bold; font-size: 12px;")
             )
           )
           ),
  tabPanel(
    "FAQs",
    tags$head(
      tags$meta(charset = "UTF-8"),
      tags$meta(name = "viewport"),
      tags$title("Solar Farm FAQs"),
      tags$style(
        HTML(
          "
            html, body {
            font-family: Arial, sans-serif;
            background-color: #0B2341; /* Set background color for the entire body */
            color: #FF8200;
            margin: 0; /* Remove default margin */
            padding: 0; /* Remove default padding */
            }

            header {
                text-align: center;
                background-color: #0B2341;
                color: #FF8200;
                padding: 20px;
                margin-top: -20px;
            }

            main {
                max-width: 10000px;
                margin: 0 auto;
                padding: 0px;
            }

            section {
                margin-bottom: 40px;
            }

            h2 {
                font-size: 24px;
                margin-bottom: 0px;
            }

            ul {
                list-style-type: none;
                padding: 0;
            }

            li {
                margin-bottom: 20px;
                padding: 10px;
                border: 0px solid #ccc;
                border-radius: 0px;
                transition: background-color 0.3s ease;
            }

            li:hover {
                background-color: transparent;
            }

            nav ul {
                list-style-type: none;
                padding: 0;
                text-align: center;
            }

            nav li {
                display: inline;
                margin-right: 20px;
            }
            
            .faq-item {
                display: flex;
                flex-direction: column;
                align-items: left;
                text-align: left;
                font-size: 18px;
                color: #0B2341;
            }
            "
        )
      )
    ),
    tags$body(
      tags$header(
        tags$h1("Solar Farm FAQs")
      ),
      tags$main(
        tags$nav(
          tags$ul(
            tags$li(tags$a(href = "#solar-panels", "Solar Panels")),
            tags$li(tags$a(href = "#energy-production", "Energy Production")),
            tags$li(tags$a(href = "#solar-farms", "Solar Farms")),
            tags$li(tags$a(href = "#irradiance", "Irradiance"))
          )
        ),
        tags$div(
          class = "faq-item",
          tags$h2(id = "solar-panels", "Solar Panels"),
          tags$ul(
            tags$li(
              tags$h3("How do solar panels work?"),
              tags$p(
                "Solar panels work by capturing sunlight and converting it into electricity through a process called the photovoltaic effect. Each solar panel consists of solar cells made of semiconductor materials, usually silicon, which generate a flow of electrons when exposed to sunlight. This flow of electrons creates direct current (DC) electricity, which is then converted into usable alternating current (AC) electricity through an inverter."
              )
            ),
            tags$li(
              tags$h3("What are the different types of solar panels?"),
              tags$p(
                "There are several types of solar panels, including monocrystalline, polycrystalline, and thin-film. Monocrystalline panels are known for their efficiency and sleek appearance, while polycrystalline panels are cost-effective. Thin-film panels are lightweight and flexible, making them suitable for specific applications."
              )
            )
          )
        ),
        tags$div(
          class = "faq-item",
          tags$h2(id = "energy-production", "Energy Production"),
          tags$ul(
            tags$li(
              tags$h3("How much energy can a solar farm produce?"),
              tags$p(
                "The energy production of a solar farm depends on several factors, including its size, location, efficiency of solar panels, and available sunlight. On average, a well-designed solar farm can generate enough electricity to power hundreds or even thousands of homes."
              )
            ),
            tags$li(
              tags$h3("What is the lifespan of solar panels?"),
              tags$p(
                "Solar panels typically have a lifespan of 25 to 30 years. However, they can continue to produce electricity at a reduced efficiency beyond that period. Regular maintenance and monitoring can help maximize their lifespan."
              )
            )
          )
        ),
        tags$div(
          class = "faq-item",
          tags$h2(id = "solar-farms", "Solar Farms"),
          tags$ul(
            tags$li(
              tags$h3("What is the role of a solar farm in renewable energy?"),
              tags$p(
                "Solar farms play a crucial role in generating clean and renewable energy. They harness sunlight to produce electricity without emitting greenhouse gases, contributing to a reduction in carbon emissions and combating climate change. Solar farms also support energy diversification and reduce dependence on fossil fuels."
              )
            ),
            tags$li(
              tags$h3("How are solar farms designed for optimal energy production?"),
              tags$p(
                "Solar farms are strategically designed to maximize energy production. Factors such as panel orientation, tilt angle, and spacing between panels are optimized to capture the most sunlight throughout the day. Additionally, advanced tracking systems can follow the sun's path for even greater efficiency."
              )
            )
          )
        ),
        tags$div(
          class = "faq-item",
          tags$h2(id = "irradiance", "Irradiance"),
          tags$ul(
            tags$li(
              tags$h3("What is solar irradiance and how does it affect energy production?"),
              tags$p(
                "Solar irradiance refers to the amount of sunlight energy received per unit area. It plays a critical role in energy production because the intensity of sunlight directly impacts the electricity output of solar panels. Higher irradiance levels lead to greater energy generation, while factors like shading and weather conditions can reduce irradiance."
              )
            ),
            tags$li(
              tags$h3("How is solar irradiance measured and monitored?"),
              tags$p(
                "Solar irradiance is measured using instruments called pyranometers or solar radiometers. These devices quantify the amount of solar energy reaching the Earth's surface. In solar farms, multiple sensors are often deployed to monitor irradiance levels continuously, allowing for real-time adjustments to maximize energy production."
              )
            )
          )
        ),
      ),
      tags$footer(
        div(
          style = "background-color: #0b2341; color: #F5F7FA; text-align: center; padding: 20px; display: flex; flex-direction: row; justify-content: space-between; align-items: center;",
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
          div(
            h5(
              "This project is supported by the University of Tennessee Research Foundation, the Department of Computer Science, and the Department of Mathematics and Statistics at the University of Tennessee at Martin.",
              style = "font-weight: bold; font-size: 12px;"
            )
          )
        )
      )
    )
),
)
# Define server logic
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
  
  #Initializes PowerProduction button to regular data
  PowerProduction <- reactiveVal(FALSE)
  currentButtonText <- reactiveVal("Irradiance")
  #When the button is clicked, change states
  observeEvent(input$togglePowerProductionButton, {
    PowerProduction(!PowerProduction())
    if(currentButtonText() == "Irradiance") {
      currentButtonText("Power Production")
    } else {
      currentButtonText("Irradiance")
    }
    updateActionButton(session, "togglePowerProductionButton", label = currentButtonText())
  })
  
  #If the button is in a PowerProduction state, then output the PowerProduction data
  PowerProduction_data <- reactive({
    data <- csv_data()
    if (PowerProduction()) {
      data$PowerProductionValue <- max(0, -0.335 + 0.058 * data$irradiance)
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
        #Checks whether it is in a PowerProduction state
        if(PowerProduction()) {
          # Detrend the data
          PowerProduction_sensor <- max(0, filtered_data[[column_name]] * 0.058 - 0.335)
          
          #Converts data to manipulable format
          p_data <- data.frame(MINUTE = filtered_data$MINUTE, PowerProductionValue = PowerProduction_sensor)
          
          # Create the plot with MINUTE on the X-axis and PowerProduction sensor irradiance on the Y-axis
          p <- ggplot(data = p_data, aes(x = MINUTE, y = PowerProductionValue)) +
            geom_line() +
            labs(
              title = paste(marker_label, "Power Production", format(input$Date, "%m-%d-%Y")),
              x = "Time (Hours)",
              y = paste(marker_label, "Power Production (W)")
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
          #If not in PowerProduction state, plot regular data
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
      
      if (PowerProduction()) {
        if (PowerProduction()) {
          # Detrend the data
          PowerProduction_sensor <- filtered_data[, column_name] * 0.20
          
          #Converts to manipulable data
          p_data <- data.frame(MINUTE = filtered_data$MINUTE, PowerProductionValue = PowerProduction_sensor)
          
          # Create the plot with MINUTE on the X-axis and PowerProduction sensor irradiance on the Y-axis
          p <- ggplot(data = p_data, aes(x = MINUTE, y = PowerProductionValue)) +
            geom_line() +
            labs(
              title = paste(marker_label, "Power Production", format(input$Date, "%m-%d-%Y")),
              x = "Time (Hours)",
              y = paste(marker_label, "Power Production (W)")
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
        #If button is not in PowerProduction state
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
  
  total_production <- reactive({
    data <- csv_data()
    daily <- sum(data[data$DOY == max(data$DOY), "SensorSum"])
    weekly <- sum(data[data$DOY >= max(data$DOY) - 6 & data$DOY <= max(data$DOY), "SensorSum"])
    monthly <- sum(data[data$DOY >= max(data$DOY) - 30 & data$DOY <= max(data$DOY), "SensorSum"])
    return(list(daily = daily, weekly = weekly, monthly = monthly))
  })
  
  output$total_production_text <- renderUI({
    daily_sum <- total_production()$daily
    weekly_sum <- total_production()$weekly
    monthly_sum <- total_production()$monthly
    
    HTML(paste(
      "The West Tennessee Solar Farm has powered", floor(daily_sum / 30), "houses or ", floor(daily_sum / 11.81), " electric cars today.",
      "The West Tennessee Solar Farm has powered", floor(weekly_sum / 30), "houses or ", floor(weekly_sum / 11.81), " electric cars in the past 7 days.",
      "The West Tennessee Solar Farm has powered", floor((weekly_sum * 4) / 30), "houses or ", floor((weekly_sum * 4) / 11.81), " electric cars in the past 31 days."
    ))
  })
  #When closing out of the browser, the app automatically stops
  session$onSessionEnded(stopApp)
}

# Run the Shiny app
shinyApp(ui = ui, server = server)