# Revitalizing Solar Insights: A Dashboard for West Tennessee Solar Farm
Joshua Chamberlain and Andy Lum

## Description
Welcome to the GitHub repository for our senior design class project! Our project aims to construct an interactive dashboard to visualize and retrieve solar irradiance data collected at the West Tennessee Solar Farm near Stanton, Tennessee.
The University of Tennessee Research Foundation is interested in revitalizing this site due to its proximity to the new Ford plant. Currently, the interactive dashboard located on the Farm’s website does not work. We plan to make use of
R, Shiny, Python, and SQL. The plan is to use simulated solar irradiance data and a hypothetical layout of sensors at the plant. Overall, we will make use of Python to simulate real-time data collection and use R and Shiny to create the dashboard. This dashboard will then be incorporated into a dedicated website, ensuring its widespread availability to the general public.

## Technologies
* MySQL
  - Stores and manages sensor data in a table containing 4 columns: `Sensor_id`, `timestamp`, `date`, and `irradiance`.
  - Connects to Python program.
* Python
  - Retrieves data from the MySQL database and updates a Google Drive CSV for data simulation.
* Google Drive API
  - Hosts the CSV file remotely and enhances data security.
* R-Shiny
  - Develops an interactive dashboar for data visualization.
    * Key Functionalities:
      * Real-Time Data Visualization
        - live updayes of solar farm sensor data with periodic refreshing.
      * Interactive Maps
        - Utilizes Leaflet to create an interactive map with clickable sensor markers, providing real-time and historical data insights.
      * Sensor Information Panel
        - Displays real-time data when a sensor is clicked on the map.
      * Data Visualization
        - Dynamic visualizations for sensor data trends over time.
      * Historical Data Analysis
        - Plots daily, weekly, and monthly data based on user-selected dates.
      * User-Friendly Interface
        - Intuitive dashboard designed for easy exploration and analysis.
      * Responsive Design
        - Ensure the dashboard layout is usable on various devices and screen dimensions, ensuring usability and optimal functionality across platforms.
      * Dashboard Hosting
        - Deploys the dashboard on shinyapps.io for wider accessibility.
      * Export Data
        - Allows users to download sensor data or visualizations in various formats (CSV, PDF, etc.).
* Shinyapps.io
  - Hosts a web server to allow users from all major operating systems to be able to access the dashboard.
* Google Cloud Console
  - Safeguards API information for enhanced data security.
