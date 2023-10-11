# Revitalizing Solar Insights: A Dashboard for West Tennessee Solar Farm
Joshua Chamberlain and Andy Lum

## Abstract
This project we constructed aims to be an interactive dashboard for displaying solar irradiance data collected at a photovoltaic power station. Given a recent push by the University of Tennessee Research Foundation toward revitalizing its use, the West Tennessee Solar Farm will serve as a template. This location is of particular interest due to its proximity to Blue Oval City (the site of the new Ford manufacturing plant, near Stanton, TN). With the farm’s existing dashboard being non-functional, there is a demand for a solution, which we will achieve through MySQL, Python, Google Drive API, R-Shiny, Shinyapps.io, and Google Cloud Console.
MySQL serves as our data hub, efficiently organizing solar energy data by sensor location. Python, coupled with the Google Drive API, simulates real-time data collection. The core of the project is an R-Shiny dashboard offering real-time data visualization, interactive maps, detailed sensor information, and access to historical data and analysis. Users can select their desired time frames. Shinyapps.io hosts the dashboard, ensuring accessibility across diverse platforms, such as web browsers and various operating systems. This approach allows users from all major operating systems to access the dashboard, promoting widespread accessibility. To further fortify data security and enhance user convenience, Google Cloud Console safeguards our API information.
Our dashboard incorporates an export function, enabling users to extract data. In addition, we constructed an easy-to-use webpage that is accessible across various major operating systems. This approach ensures that our project is widely available and caters to a diverse audience; thus, making valuable solar irradiance data easily accessible to all. This project aims to provide researchers, policymakers, and the public with real-time insights into solar irradiance data at the West Tennessee Solar Farm, supporting sustainable energy solutions. 


## Technologies
* __MySQL__
  - Stores and manages sensor data in a table containing columns: `DOY (Day of Year)`, `MINUTE`, `Sensor1`, `Sensor2`, `Sensor3`, `Sensor4`, `Sensor5`, `Sensor6`, `Sensor7`, `Sensor8`, `Sensor9`,and `Sensor10`.
  - Connects to Python program.
* __Python__
  - Retrieves data from the MySQL database and updates a Google Drive CSV for data simulation.
* __Google Drive API__
  - Hosts the CSV file remotely and enhances data security.
* __R-Shiny__
  - Develops an interactive dashboard for data visualization.
    * __Key Functionalities__:
      1. __Real-Time Data Visualization__
        * live updates of solar farm sensor data with periodic refreshing.
      2. __Interactive Maps__
        * Utilizes Leaflet to create an interactive map with clickable sensor markers, providing real-time and historical data insights.
      3. __Sensor Information Panel__
        * Displays real-time data when a sensor is clicked on the map.
      4. __Data Visualization__
        * Dynamic visualizations for sensor data trends over time.
      5. __Historical Data Analysis__
        * Plots daily, weekly, and monthly data based on user-selected dates.
      6. __User-Friendly Interface__
        * Intuitive dashboard designed for easy exploration and analysis.
      7. __Responsive Design__
        * Ensure the dashboard layout is usable on various devices and screen dimensions, ensuring usability and optimal functionality across platforms.
      8. __Dashboard Hosting__
        * Deploys the dashboard on shinyapps.io for wider accessibility.
      9. __Export Data__
        * Allows users to download sensor data or visualizations in various formats (CSV, PDF, etc.).
* __Shinyapps.io__
  - Hosts a web server to allow users from all major operating systems to be able to access the dashboard.
* __Google Cloud Console__
  - Safeguards API information for enhanced data security.

## Goals
* Establish a streamlined data flow from the mySQL server to the python code, then to the Google Drive in CSV format, and finally to the R-shiny dashboard, ensuring efficiency and a cohesive data pipeline.
* Establish a robust and automated mechanism within the Python script that consistently updates the Google Drive CSV file at defined intervals. This process ensures accurate representation of real-time sensor data changes while incorporating error-handling mechanisms to effectively handle potential issues that might arise during updates.
* Implement a R-Shiny Dashboard that encompasses all of the aforementioned functionalities.
* Develop a webpage that ensures accessibility across a variety of major operating systems on laptops, making it available and user-friendly for all users.

## Stretch Goals
* Incorporate a video tutorial that effectively guides a user through the dashboard’s navigation process.
* Comparison of data from multiple sensors by enabling users to select two or more sensors and generate individual graphs for each of the chosen sensors side-by-side.
* Allow integration of external APIs to provide more real time location data. For example, using a weather API to display what type of weather conditions are over each sensor.
