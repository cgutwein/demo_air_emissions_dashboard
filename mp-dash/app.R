library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggthemes)
#library(ggmap)
library(maps)
library(mapdata)
library(lubridate)
library(RPostgreSQL)
library(plotly)
library(RColorBrewer)
library(leaflet)
library(htmltools)

# Initialize db connections
# drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, user="postgres", host="34.69.75.112", dbname="postgres", password="postgres")
# pg_init <- paste("SELECT * FROM sensor_data1 WHERE created_on BETWEEN '", today()-1, "' AND '", today(), "' AND sensor_id = ", 7956, ";", sep="")
# data <- dbGetQuery(con, pg_init)
Sys.setenv(TZ="UTC")
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
points <- cbind.data.frame(lng = c(-104.988855, -88.08802, -87.629436, -93.217521, -80.659763, -87.763718),
                           lat = c(39.785917, 41.863506, 41.86892, 44.935817, 35.437746, 42.0225),
                           id = c(7956, 4838, 5588, 3088, 1391, 4404),
                           name = c("Globeville (Denver, CO)", "Wheaton Highlands (Elmhurst, IL)", "1138 Plymouth (Chicago, IL)", "Howe Neighborhood (Mendota Heights, MN)", "Concord, NC", "Mulford Manor (Evanston, IL)"), stringsAsFactors = FALSE)
# Choices for drop-downs
vars <- c(
  "PM2.5" = "pm25",
  "PM10" = "pm10",
  "PM1" = "pm1"
)

## PM AQI Table and function
aqi_levels <- c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous")
aqi_colors <- c('green', 'yellow', 'orange', 'red', 'purple', 'maroon')
aqi_messages <- c('Air quality is considered satisfactory, and air pollution poses little or no risk.',
                  'Air quality is acceptable; however, for some pollutants there may be a moderate health concern for a very small number of people. For example, people who are unusually sensitive to ozone may experience respiratory symptoms.',
                  'Although general public is not likely to be affected at this AQI range, people with lung disease, older adults and children are at a greater risk from exposure to ozone, whereas persons with heart and lung disease, older adults and children are at greater risk from the presence of particles in the air.',
                  'Everyone may begin to experience some adverse health effects, and members of the sensitive groups may experience more serious effects.',
                  'This would trigger a health alert signifying that everyone may experience more serious health effects.',
                  'This would trigger a health warnings of emergency conditions. The entire population is more likely to be affected.')
aqi_master <- cbind.data.frame(aqi_levels, aqi_colors, aqi_messages, stringsAsFactors = FALSE)

## function to determine AQI factor level based on AQI value
aqi_factorize <- function(aqi_value) {cut(aqi_value, c(-1, 50, 100, 150, 200, 300, 500), labels = FALSE)}

cols = c("category", "c_high", "c_low", "aqi_high", "aqi_low")
pm_table = data.frame(
  c("Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
  c(12, 35.4, 55.4, 150.4, 250.4, 500.4),
  c(0, 12, 35.4, 55.4, 150.4, 250.4),
  c(50, 100, 150, 200, 300, 500),
  c(0, 50, 100, 150, 200, 300)
)
colnames(pm_table) <- cols

# function to convert ug/m3 to AQI
pm_aqi <- function(C){
  if (C > 500) {return(500)}
  else {
    for (i in 1:6) {
      if ((C >= pm_table$c_low[i]) & (C <= pm_table$c_high[i])) {
        return( ((pm_table$aqi_high[i] - pm_table$aqi_low[i]) / 
                   (pm_table$c_high[i] - pm_table$c_low[i])) * (C - pm_table$c_low[i]) + pm_table$aqi_low[i])
      }
    }
  }
}

q_data <- function(q){
  drv <- dbDriver("PostgreSQL")
  con <- dbConnect(drv, user="postgres", host="35.202.165.5", dbname="postgres", password="postgres")
  temp_df <- dbGetQuery(con, q)
  dbDisconnect(con)
  dbUnloadDriver(drv)
  return (temp_df)
}

## Function to calculate position for gauge needle
needle_pos <- function(aqi_val){
  if (aqi_val >= 250) {
    x_val <- (0.50 + 0.3*sin(90*pi/180*(aqi_val-250)/250))
    y_val <- (0.50 + 0.3*sin(90*pi/180*(500-aqi_val)/250))
    return(paste('M 0.485 0.5 L', x_val, y_val, 'L 0.515 0.5 Z'))}
  else {
    x_val <- (0.50 - 0.3*sin(90*pi/180*(250-aqi_val)/250))
    y_val <- (0.50 + 0.3*sin(90*pi/180*(aqi_val)/250))
    return(paste('M 0.485 0.5 L', x_val, y_val, 'L 0.515 0.5 Z'))}
}


ui <- dashboardPage(skin="blue",
                    # Give the page a title
                    dashboardHeader(title = tags$a(href='http://www.mostardiplatt.com',
                                                   tags$img(src='logo.png', width='75%'), "AQI Dashboard")),
                    
                    # Generate a row with a sidebar
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Sensor Data", tabName = "sensor_data", icon = icon("stats", lib = "glyphicon")),
                        #menuItem("Map", tabName = "sensor_map", icon = icon("globe", lib = "glyphicon")),
                        menuItem("Information", tabName = "info", icon = icon("info-sign", lib = "glyphicon"))
                      )
                    ),
                    
                    
                    # Create a spot for the barplot
                    dashboardBody(
                      tabItems(
                        # first tab item
                        tabItem(tabName = "sensor_map",
                                tags$style(type = "text/css", "#map1 {height: calc(100vh - 80px) !important;}"),
                                leafletOutput("map1")
                        ),
                        # second tab item
                        tabItem(tabName = "sensor_data",
                                fluidRow(box(width=3, dateInput("date1", "Begin Date:", value=Sys.time()-86400),
                                             # Default value is the date in client's time zone
                                             dateInput("date2", "End Date:", value=Sys.time()),
                                             hr(),
                                             selectInput("Sid", "Sensor ID:", c("Lincoln Yards (Chicago, IL)" = 12981,
                                                                                "Mulford Manor (Evanston, IL)" = 4404,
                                                                                "Globeville (Denver, CO)" = 7956, 
                                                                                "Howe Neighborhood (Mendota Heights, MN)" = 3088, 
                                                                                #"1138 Plymouth (Chicago, IL)" = 5588, 
                                                                                "Concord, NC" = 1491)),
                                             helpText("This dashboard displays information about and data collected by Purple Air sensors deployed near each regional branch office
                                                      of Mostardi Platt. Use the date and sensor selectors above to modify the data selection. The time series plot below is
                                                      interactive - use the pan, zoom and other functions to gain insights.\nCreated by Mostardi Platt\nData provided by: Purple Air")),
                                         #valueBoxOutput("aqiBox"),
                                         box(plotlyOutput("gauge"),
                                             textOutput("aqiMessage"), width=5),
                                         box(plotlyOutput("scatter1"), width=4)),
                                
                                fluidRow(box(plotlyOutput("time_series1"), width=12)),
                                fluidRow(box(plotlyOutput("time_series2"), width=6),
                                         box(plotlyOutput("time_series3"), width=6))
                                ),
                        # info tab item
                        tabItem(tabName = "info",
                                tags$h1("Real Time Air Sensor Data Dashboard"),
                                tags$p("Mostardi Platt is a trusted leader in providing high quality environmental services to a wide variety of industries and applications. As part of its ongoing effort to deploy, test, and manage low cost air sensor data, we have created this demo dashboard to showcase our ability to provide interactive presentation of real-time data."),
                                tags$h2("Data"),
                                tags$p("The data presented is provided by Purple Air sensors. We have selected a handful of sensors from the network of ~9,500 Purple Air sensors and have been collecting data approximately every 80 seconds from these sensors."),
                                tags$p("Looking forward, we plan to incorporate public data from Chicago's Array of Things project."),
                                tags$h2("Contact"),
                                tags$p("Please feel free to reach out to the Compliance Management division of Mostardi Platt to learn more about our work with low cost sensors."),
                                tags$a(href="mailto:jpowell@mp-mail.com", "Jim Powell, Compliance Management Lead")
                        )
                        
                                )
                      
)
)  


server <- function(input, output) {
  
  #map query to get 24 hr data for each sensor, and calculate 24 hr, 1 hr, and current AQI
  ###
  
  map_query <- reactive({
    paste("SELECT AVG(b_pm25_atm) as avg_25b, AVG(a_pm25_atm) as avg_25a,sensor_id FROM sensor_data1 WHERE created_on BETWEEN '", Sys.time() - 86400, "' AND '", Sys.time(), "' GROUP BY sensor_id;", sep="")
  })
  
  map_query_1 <- reactive({
    paste("SELECT AVG(b_pm25_atm) as avg_25b, AVG(a_pm25_atm) as avg_25a,sensor_id FROM sensor_data1 WHERE created_on BETWEEN '", Sys.time() - 3600, "' AND '", Sys.time(), "' GROUP BY sensor_id;", sep="")
  })
  
  map_query_c <- reactive({
    paste("SELECT AVG(b_pm25_atm) as avg_25b, AVG(a_pm25_atm) as avg_25a,sensor_id FROM sensor_data1 WHERE created_on BETWEEN '", Sys.time() - 180, "' AND '", Sys.time(), "' GROUP BY sensor_id;", sep="")
  })
  
  #data for map tiles
  map_data <- reactive({
    q_data(map_query())
  })
  
  map_data_1 <- reactive({
    q_data(map_query_1())
  })
  
  map_data_c <- reactive({
    q_data(map_query_c())
  })
  
  icons <- reactive({
    aq_value <- round(map_data()$avg_25a, 0)
    use_this_color <- aqi_master$aqi_colors[aqi_factorize(aq_value)]
    
    return(awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = "green"
    ))
  })
  
  #map output
  output$map1 <- renderLeaflet({
    
    leaflet(points) %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 5) %>%
      addMarkers(~lng, ~lat, icon=icons, popup=~htmlEscape(name))
  })
  
  # from selectors on Sensor tab page, generate query to pull from database
  # defaults to Mulford Manor for last 24 hours
  ##
  
  pg_query <- reactive({
    paste("SELECT * FROM sensor_data1 WHERE created_on BETWEEN '", input$date1, "' AND '", input$date2+1, "' AND sensor_id = ", input$Sid, ";", sep="")
  })
  
  # Aggregate data based on input
  data_ <- reactive({
    temp_df <- q_data(pg_query())
    temp_df$aqi <- sapply(temp_df$a_pm25_atm, pm_aqi)
    temp_df$created_on <- temp_df$created_on - 6*3600
    return(temp_df)
  })
  
  # Fill in the spot we created for a plot
  
  output$time_series1 <- renderPlotly({
    
    # Render a plotly time series plot
    plot_ly(data_(), x = ~created_on, y = ~a_pm1_atm, name = 'PM1 (ug/dscm) Sensor A',type = 'scatter',  mode = 'lines', fill = 'tozeroy') %>%
      add_trace(y= ~a_pm25_atm, name = 'PM2.5 (ug/dscm) Sensor A', mode = 'lines', fill = 'tonexty') %>%
      add_trace(y = ~a_pm10_atm, name = 'PM10 (ug/dscm) Sensor A', mode = 'lines', fill = 'tonexty', fillcolor='rgba(71, 107, 107,0.2)', line = list(color = 'rgba(71, 107, 107,1)')) %>%
      #add_trace(y = ~`4-Black`, name = 'System 4', mode = 'lines', line = list(color = '#000000')) %>%
      layout(title = "Ambient Level PM Concentration",
             xaxis = list(
               rangeselector = list(
                 buttons = list(
                   list(
                     count = 6,
                     label = "6 hr",
                     step = "hour",
                     stepmode = "backward"),
                   list(
                     count = 24,
                     label = "24 hr",
                     step = "hour",
                     stepmode = "backward"),
                   list(
                     count = 30,
                     label = "30 min",
                     step = "minute",
                     stepmode = "backward"),
                   list(step = "all"))),
               rangeslider = list(type = "date")),
             yaxis = list (title = "Concentration (ug/dscm)"))
  })
  
  output$time_series2 <- renderPlotly({
    
    # Render a plotly time series plot
    plot_ly(data_(), x = ~created_on, y = ~temp, name = 'Temperature (F)',type = 'scatter',  mode = 'lines', fill = 'tozeroy') %>%
      layout(title = "Temperature",
             yaxis = list (title = "Temperature (F)"))
  })
  
  output$time_series3 <- renderPlotly({
    
    # Render a plotly time series plot
    plot_ly(data_(), x = ~created_on, y = ~humidity, name = 'Humidity (%)',type = 'scatter',  mode = 'lines', fill = 'tozeroy') %>%
      layout(title = "Humidity",
             yaxis = list (title = "Humidity (%)"))
  })
  
  output$scatter1 <- renderPlotly({
    
    # Render a multiple barplot in ggplot2
    a <- list(
      x = max(data_()$a_pm25_atm/2),
      y = max(data_()$b_pm25_atm/2),
      text = paste("R^2 = ", round(summary(lm(b_pm25_atm ~ a_pm25_atm, data=data_()))$r.squared, 3)),
      showarrow = FALSE,
      xref = "x",
      yref = "y",
      ax = 20,
      ay = -40,
      font = list(color = 'blue',
                  size = 14,
                  bold=TRUE)
    )
    
    plot_ly(
      data_(), type = 'scatter',
      mode='markers', x = ~a_pm25_atm, y = ~b_pm25_atm,
      color = ~a_pm25_atm)%>%
      layout(annotations = a)
  })
  
  output$aqiBox <- renderValueBox({
    # 1 week, 24 hr, 1 hr, and current PM2.5 AQI times
    # 
    
    
    valueBox(
      value = 25,
      subtitle = "24 hr Air Quality Index",
      icon = icon("area-chart"),
      color = if (25 >= 50) "yellow" else "green"
    )
  })
  
  output$gauge <- renderPlotly({
    aq_value <- round(pm_aqi(map_data()$avg_25a[map_data()$sensor_id == input$Sid]), 0)
    use_this_color <- aqi_master$aqi_colors[aqi_factorize(aq_value)]
    use_this_message <- aqi_master$aqi_levels[aqi_factorize(aq_value)]
    
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, 10, 3,5,5,5,10,22),
      sort=FALSE,
      labels = c("-", "0", "50", "100", "150", "200","300", "500"),
      rotation = 108,
      direction = "clockwise",
      hole = 0.4,
      textinfo = "label",
      textposition = "outside",
      hoverinfo = "none",
      #domain = list(x = c(0, 0.48), y = c(0, 1)),
      marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)')),
      showlegend = FALSE
    )
    
    base_plot <- add_trace(
      base_plot,
      type = "pie",
      values = c(50, 5,5,5,5,10,20),
      sort = FALSE,
      labels = c("Air Quality Index", "Good", "Moderate", "Unhealthy for Sensitive Groups", "Unhealthy", "Very Unhealthy", "Hazardous"),
      rotation = 90,
      direction = "clockwise",
      hole = 0.3,
      textinfo = "label",
      textposition = "inside",
      hoverinfo = "none",
      #domain = list(x = c(0, 0.48), y = c(0, 1)),
      opacity = 0.9,
      marker = list(colors = c('rgb(255, 255, 255)', 'green', 'yellow', 'orange', 'red', 'purple', 'maroon')),
      showlegend= FALSE
    )
    
    a <- list(
      showticklabels = FALSE,
      autotick = FALSE,
      showgrid = FALSE,
      zeroline = FALSE)
    
    b <- list(
      xref = 'paper',
      yref = 'paper',
      x = 0.5,
      y = 0.25,
      showarrow = FALSE,
      text = paste('Current AQI: ', as.character(aq_value)),
      xanchor='center',
      font = list(color = 'black',
                  family = 'sans serif',
                  size = 26))
    
    c <- list(
      xref = 'paper',
      yref = 'paper',
      x = 0.5,
      y = 0.15,
      showarrow = FALSE,
      text = use_this_message,
      xanchor='center',
      font = list(color = 'black',
                  family = 'sans serif',
                  size = 14))
    
    m <- list(
      l = 50,
      r = 50,
      b = 100,
      t = 100,
      pad = 4
    )
    
    base_chart <- layout(
      base_plot,
      shapes = list(
        list(
          type = 'path',
          path = needle_pos(aq_value),
          xref = 'paper',
          yref = 'paper',
          fillcolor = use_this_color
        ),
        list(
          type = 'path',
          path = 'M 0.0 0.0 L 0.0 0.4 L 1 0.4 L 1 0.0 Z',
          xref = 'paper',
          yref = 'paper',
          fillcolor = use_this_color
        )
      ),
      xaxis = a,
      yaxis = a,
      annotations = list(b,c),
      margin = m
    )
  })
  
  output$aqiMessage <- reactive({
    aq_value <- round(pm_aqi(map_data()$avg_25a[map_data()$sensor_id == input$Sid]), 0)
    aqi_master$aqi_messages[aqi_factorize(aq_value)]
  })
  
  #   observe({
  #     leafletProxy("map1") %>% clearPopups()
  #     event <- input$map_shape_click
  #     if (is.null(event))
  #       return()
  #     
  #     isolate({
  #       content <- as.character(tagList(
  #         tags$h4("ID:", event$id),
  #         tags$br(),
  #         sprintf("Latitude: %s", event$lat), tags$br(),
  #         sprintf("Latitude: %s", event$lng)
  #       ))
  #       leafletProxy("map1") %>% addPopups(event$lng, event$lat, content)
  #     })
  #   })
}

shinyApp(ui = ui, server = server)
