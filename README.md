# Revitalizing Solar Insights: A Dashboard for West Tennessee Solar Farm
Joshua Chamberlain and Andy Lum

## Description
Welcome to the GitHub repository for our senior design class project! Our project aims to construct an interactive dashboard to visualize and retrieve solar irradiance data collected at the West Tennessee Solar Farm near Stanton, Tennessee.
The University of Tennessee Research Foundation is interested in revitalizing this site due to its proximity to the new Ford plant. Currently, the interactive dashboard located on the Farm’s website does not work. We plan to make use of
R, Shiny, Python, and SQL. The plan is to use simulated solar irradiance data and a hypothetical layout of sensors at the plant. Overall, we will make use of Python to simulate real-time data collection and use R and Shiny to create the dashboard. This dashboard will then be incorporated into a dedicated website, ensuring its widespread availability to the general public.

## Technologies
* __MySQL__
  - Stores and manages sensor data in a table containing 4 columns: `Sensor_id`, `timestamp`, `date`, and `irradiance`.
  - Connects to Python program.
* __Python__
  - Retrieves data from the MySQL database and updates a Google Drive CSV for data simulation.
* __Google Drive API__
  - Hosts the CSV file remotely and enhances data security.
* __R-Shiny__
  - Develops an interactive dashboar for data visualization.
    * __Key Functionalities__:
      1. __Real-Time Data Visualization__
        - live updayes of solar farm sensor data with periodic refreshing.
      2. __Interactive Maps__
        - Utilizes Leaflet to create an interactive map with clickable sensor markers, providing real-time and historical data insights.
      3. __Sensor Information Panel__
        - Displays real-time data when a sensor is clicked on the map.
      4. __Data Visualization__
        - Dynamic visualizations for sensor data trends over time.
      5. __Historical Data Analysis__
        - Plots daily, weekly, and monthly data based on user-selected dates.
      6. __User-Friendly Interface__
        - Intuitive dashboard designed for easy exploration and analysis.
      7. __Responsive Design__
        - Ensure the dashboard layout is usable on various devices and screen dimensions, ensuring usability and optimal functionality across platforms.
      8. __Dashboard Hosting__
        - Deploys the dashboard on shinyapps.io for wider accessibility.
      9. __Export Data__
        - Allows users to download sensor data or visualizations in various formats (CSV, PDF, etc.).
* __Shinyapps.io__
  - Hosts a web server to allow users from all major operating systems to be able to access the dashboard.
* __Google Cloud Console__
  - Safeguards API information for enhanced data security.
