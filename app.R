#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


if (!require(shiny))
  install.packages("shiny")
if (!require(tidyverse))
  install.packages("tidyverse")
if (!require(dplyr))
  install.packages("dplyr")
if (!require(leaflet))
  install.packages("leaflet")
if (!require(ggplot2))
  install.packages("ggplot2")
if (!require(forecast))
  install.packages("forecast")
if (!require(europepmc))
  install.packages("europepmc")
if (!require(lubridate))
  install.packages("lubridate")
if (!require(ggthemes))
  install.packages("ggthemes")
if (!require(shinyWidgets))
  install.packages("shinyWidgets")
if (!require())
  install.packages("DT")
if (!require(data.table))
  install.packages("data.table")
if (!require())
  install.packages("forecast")
if (!require()) 
  install.packages("TTR")

## Loading Libraries required for the analysis
library(shiny)
library(tidyverse)
library(dplyr)
library(stringr)
library(viridis)
library(leaflet)
library(ggplot2)
library(forecast)
library(europepmc)
library(lubridate)
library(ggthemes)
library(shinyWidgets)
library(data.table)
library(DT)
library(shinythemes)
library(forecast)
library(TTR)


#Reading the data
#crime <- read.csv("data/crime.csv")
crime <- read.csv("crime.csv")

#Omitting the NAs
crime <- na.omit(crime)

#Removing the latitude and longitude that do not correspond to the city of Boston
crime <- crime[!(crime$Lat == -1 & crime$Long == -1),]

#Finding min and max months for each year (for slider input)

#Count of each month for every year and sorting the data
month_count_per_year <- count(crime, MONTH, YEAR)
month_count_per_year <- month_count_per_year[with(month_count_per_year, order(YEAR, -n, MONTH)), ]
sorted_months <- month_count_per_year[with(month_count_per_year, order(YEAR, -n, MONTH)), ]

#Filtering data to get start and end months for year 2015
months_2015 <- filter(sorted_months, YEAR == "2015")
#Extracting minimum and maximum months in the year 2015
min_month_2015 <- min(months_2015[, 1])
max_month_2015 <- max(months_2015[, 1])

#Filtering data to get start and end months for year 2016
months_2016 <- filter(sorted_months, YEAR == "2016")
min_month_2016 <- min(months_2016[, 1])
max_month_2016 <- max(months_2016[, 1])

#Filtering data to get start and end months for year 2017
months_2017 <- filter(sorted_months, YEAR == "2017")
min_month_2017 <- min(months_2017[, 1])
max_month_2017 <- max(months_2017[, 1])

#Filtering data to get start and end months for year 2018
months_2018 <- filter(sorted_months, YEAR == "2018")
min_month_2018 <- min(months_2018[, 1])
max_month_2018 <- max(months_2018[, 1])

#Overall crime trends over the years
trend_by_year <- epmc_hits_trend('crime', period = 2015:2018, synonym = FALSE)

#Data for top 5 crimes each year
crimes_each_year <- count(crime, OFFENSE_CODE_GROUP, YEAR)
crimes_each_year <- as.data.frame(crimes_each_year)

#Sorting crimes by grouping it by year, number of crimes in decsending order
crime_sorted <- crimes_each_year[with(crimes_each_year, order(YEAR,-n, OFFENSE_CODE_GROUP)),]
top5_crimes_per_year <- c()
hits_year <- unique(crime_sorted$YEAR)

#Loop for selecting the top 5 crimes each year
for (x in hits_year)
{
  hits_crimes_year <- subset(crime_sorted, crime_sorted$YEAR == x)
  top5 <- head(hits_crimes_year, 5)
  top5_crimes_per_year <- rbind(top5_crimes_per_year, top5)
}

#Crimes per district
district <- table(crime$DISTRICT)
district <- na.omit(district)

#Ordering the districts in descending order
ordered_by_district <- sort(district, decreasing = TRUE)
ordered_by_district <- as.data.frame(ordered_by_district)

#Renaming the columns
colnames(ordered_by_district) <- c("District", "Offenses")

#Number of crimes for Top 5 offense group
top_5_crimes <- crime %>%
  group_by(OFFENSE_CODE_GROUP) %>%
  summarise(num_crime = n()) %>%
  arrange(desc(num_crime)) %>%
  head(5)

#Top 5 streets with number of crimes
top_5_street <- crime %>%
  group_by(STREET) %>%
  summarise(num_crime = n()) %>%
  arrange(desc(num_crime)) %>%
  head(5)

#Number of yearly crimes based on UCR_PART
ucr_year <- crime %>%
  filter(!is.na(UCR_PART)) %>%
  filter(!(UCR_PART == "")) %>%
  group_by(YEAR, UCR_PART) %>%
  summarise(crime_count = n())

#Number of monthly crimes based on UCR_PART
ucr_month <- crime %>%
  filter(!is.na(UCR_PART)) %>%
  filter(!(UCR_PART == "")) %>%
  group_by(MONTH, UCR_PART) %>%
  summarise(crime_count = n())

#Time series data
ts_crimes = ts(ucr_year$crime_count,
               start = 2015,
               frequency = 3)


#Seasonal Time series
time_series_decompose <- decompose(ts_crimes)

#------------------------------------------------------------- UI -------------------------------------------------------------

#Launching the shiny app
ui <- fluidPage(
  #Define theme
  theme = shinytheme("slate"),
  # Application title
  titlePanel("Exploratory Data Analysis "),
  titlePanel("Authors: Lahari Kuchibhotla, Tanmayi Varanasi, Suchita Negi, Stuti Sanghavi"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      helpText("Let's view the crimes on a map."),
      #allows the side panel to move as your scroll
      style = "position:fixed;width:inherit;",
      
      selectInput(
        "crime_by_hour",
        label = "Choose one of the Top Offenses",
        choices = list(
          "Larceny",
          "Investigate Person",
          "Medical Assistance",
          "Motor Vehicle Accident Response",
          "Other"
        ),
        selected = "Larceny",
        multiple = TRUE
      )
      ,
      fluidRow(column(
        3,
        radioButtons(
          "year",
          h3("Choose Year"),
          choices = list(
            "2015" = 2015,
            "2016" = 2016,
            "2017" = 2017,
            "2018" = 2018
          ),
          selected = 2015
        )
        
      )),
      sliderInput(
        inputId = "month",
        label = "Choose Month:",
        value = c(1, 12),
        min = 1,
        max = 12
      ),
      sliderTextInput(
        "day",
        "Select a Day of the Week",
        choices = c(
          "Monday",
          "Tuesday",
          "Wednesday",
          "Thursday",
          "Friday",
          "Saturday",
          "Sunday"
        ),
        selected = c("Monday"),
        animate = FALSE,
        grid = TRUE,
        hide_min_max = FALSE,
        from_fixed = FALSE,
        to_fixed = FALSE,
        from_min = NULL,
        from_max = NULL,
        to_min = NULL,
        to_max = NULL,
        force_edges = FALSE,
        width = 400,
        pre = NULL,
        post = NULL,
        dragRange = TRUE
      )
      
    ),
    
    mainPanel(
      h1("Let's Explore Crime Data in Boston"),
      textOutput("selected_var"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Crime Locations",
          br(),
          plotOutput("top5crimes"),
          br(),
          br(),
          leafletOutput("mymap"),
          br(),
          br(),
          plotOutput("district")
        ),
        tabPanel(
          "Trends",
          br(),
          plotOutput("monthly_trend"),
          br(),
          br(),
          plotOutput("crimes_by_hour"),
          br(),
          br(),
          plotOutput("overall_trend")
        ),
        tabPanel(
          "Monthly UCR Data",
          br(),
          textOutput("ucr"),
          br(),
          br(),
          plotOutput("ucr_monthly"),
          br(),
          br(),
          DT::dataTableOutput("table_month")
        ),
        tabPanel(
          "Yearly UCR Data",
          br(),
          plotOutput("ucr_yearly"),
          br(),
          br(),
          DT::dataTableOutput("table_year")
        ),
        tabPanel(
          "Forecast",
          br(),
          plotOutput("ts_year"),
          br(),
          br(),
          plotOutput("tsdecom")
        )
      ),
      tags$style(
        HTML(
          "
          .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate, .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
          color: #ffffff;
          }
          ### ADD THIS HERE ###
          .dataTables_wrapper .dataTables_paginate .paginate_button{box-sizing:border-box;display:inline-block;min-width:1.5em;padding:0.5em 1em;margin-left:2px;text-align:center;text-decoration:none !important;cursor:pointer;*cursor:hand;color:#ffffff !important;border:1px solid transparent;border-radius:2px}
          
          ###To change text and background color of the `Select` box ###
          .dataTables_length select {
          color: #0E334A;
          background-color: #0E334A
          }
          
          ###To change text and background color of the `Search` box ###
          .dataTables_filter input {
          color: #0E334A;
          background-color: #0E334A
          }
          
          thead {
          color: #ffffff;
          }
          
          tbody {
          color: #000000;
          }
          
          "
        )
        )
        )
        )
        )

#------------------------------------------------------------- Server-------------------------------------------------------------

server <- function(input, output, session) {
  #tab 1
  #top 5 crimes for each year
  output$top5crimes <- renderPlot({
    ggplot(data = top5_crimes_per_year,
           aes(x = YEAR, y = n, fill = OFFENSE_CODE_GROUP)) +
      geom_col(position = position_dodge()) +
      labs(title = "Top 5 Crimes Commited each Year",
           y = "Number of Crimes Committed",
           x = "Year") + theme_bw(base_size = 10) +
      scale_fill_manual(
        "Offense",
        values = c(
          "Drug Violation" = "red",
          "Investigate Person" = "orange",
          "Larceny" = "darkgreen",
          "Medical Assistance" = "yellow",
          "Motor Vehicle Accident Response" = "blue",
          "Other" = "black",
          "Simple Assault" = "grey",
          "Vandalism" = "magenta"
        )
      )
  })
  
  #total number of crimes per districtin descending order 
  districtplot <- reactive({
    off <- subset(ordered_by_district, Offenses == input$offense)
    tbl <- with(off, table(District, Offenses))
    tbldf <- data.frame(tbl)
    colnames(tbldf) <- c("District", "Offenses")
  })
  
  #plot of crimes for district
  output$district <- renderPlot({
    ggplot(data = ordered_by_district,
           main = "Deaths for each Generation",
           aes(x = District, y = Offenses)) +
      geom_col(position = position_dodge()) +
      labs(title = "Crimes Commited per District",
           y = "Number of Crimes Committed",
           x = "District") + theme_bw(base_size = 10)
  })
  
  #if the user selects multiple offenses to view them the circle markers will be different colors
  pal <- colorFactor(
    palette = c('red', 'green', 'navy', 'purple', 'black'),
    domain = top_5_crimes$OFFENSE_CODE_GROUP
  )
  
  #Geospatial map of the crimes
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Stamen.Terrain,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(color = ~ pal(OFFENSE_CODE_GROUP),
                       data = crimebyhour())
  })
  
  #tab 2
  
  #Storing the minimum and maximum values of the month the user selects on the slider for each year
  observe({
    if (input$year == "2015") {
      updateSliderInput(
        session,
        "month",
        min = min_month_2015,
        max = max_month_2015,
        value = c(min_month_2015, max_month_2015)
      )
    } else if (input$year == "2018") {
      updateSliderInput(
        session,
        "month",
        min = min_month_2018,
        max = max_month_2018,
        value = c(min_month_2018, max_month_2018)
      )
    } else{
      updateSliderInput(
        session,
        "month",
        min = 1,
        max = 12,
        value = c(1, 12)
      )
    }
  })
  
  #Storing data for each year into separate tables for plotting 
  filteredData <- reactive({
    if (input$year == "2015") {
      s1 <-
        subset(crime,
               YEAR == input$year &
                 MONTH > min_month_2015 - 1 & MONTH < max_month_2015 + 1)
      t1 <- with(s1, table(YEAR, MONTH))
      axisRange <-
        t1[, which(colnames(t1) == input$month[1]):which(colnames(t1) == input$month[2])]
    } else if (input$year == "2018") {
      s1 <-
        subset(crime,
               YEAR == input$year &
                 MONTH > min_month_2018 - 1 & MONTH < max_month_2018 + 1)
      t1 <- with(s1, table(YEAR, MONTH))
      axisRange <-
        t1[, which(colnames(t1) == input$month[1]):which(colnames(t1) == input$month[2])]
    } else{
      s1 <- subset(crime, YEAR == input$year & MONTH > 0 & MONTH < 13)
      t1 <- with(s1, table(YEAR, MONTH))
      axisRange <-
        t1[, which(colnames(t1) == input$month[1]):which(colnames(t1) == input$month[2])]
    }
  })
  
  #plotting the number of death per month for each year
  output$monthly_trend <- renderPlot({
    barplot(
      filteredData(),
      beside = TRUE,
      main = "Number of Deaths Per Month",
      xlab = "Month",
      ylab = "Number of Deaths",
      legend = FALSE,
      col = terrain.colors(12)
    )
  })
  
  
  #a breakdown of the crimes per hour
  crimebyhour <- reactive({
    hr <-
      subset(
        crime,
        OFFENSE_CODE_GROUP == input$crime_by_hour &
          YEAR == input$year &
          MONTH == input$month & DAY_OF_WEEK == input$day
      )
  })
  
  
  # Plot data for number of top 5 crimes each hour.
  output$crimes_by_hour <- renderPlot({
    crimebyhour() %>%
      inner_join(top_5_crimes, by = "OFFENSE_CODE_GROUP") %>%
      group_by (OFFENSE_CODE_GROUP, HOUR) %>%
      summarise(count = n()) %>%
      ggplot (aes(x = HOUR, y = count, fill = OFFENSE_CODE_GROUP)) +
      geom_bar(stat = "identity") +
      ggtitle ("Number of crimes per hour by top 5 crimes") +
      theme_bw()
  })
  
  
  
  #plots the trend for all crimes for 2015-2018
  output$overall_trend <- renderPlot({
    ggplot(trend_by_year, aes(year, all_hits)) + geom_point() + geom_line() +
      xlab("Year") +
      ylab("Total Number of Crimes") + ggtitle("Total Number of Crimes each Year")
  })
  
  #tab 3
  output$ucr <- renderText({
    paste(
      "Crimes in the United States are categorized by the Uniform Crime Reporting (UCR)
      index. Part 1 crimes are serious offenses such as aggravated assult, homicide, etc.
      Part 2 crimes are less severe such as embezzelment or fraud.
      Part 3 crimes are the least serious offenses such as vandalism, property investigation, etc."
    )
  })
  # Plot data for monthly crime based on Ucr parts
  output$ucr_monthly <- renderPlot({
    ggplot(ucr_month,
           aes(
             x = MONTH,
             y = crime_count ,
             fill = UCR_PART ,
             colour = UCR_PART
           )) + geom_point(size = 2) + geom_line(size = 1) +
      xlab("Month") +
      ylab("Total Number of Crimes") + ggtitle("Crimes based on UCR Part")
  })
  
  #table with UCR crime information
  output$table_month <- DT::renderDataTable({
    #DT::datatable(ucr_month2[, input$show_vars, drop = FALSE])
    DT::datatable(ucr_month, options = list(orderClasses = TRUE)) %>%
      formatStyle(
        columns = "MONTH",
        target = "cell",
        color = "orange",
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "UCR_PART",
        target = "cell",
        color = "teal",
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "crime_count",
        target = "cell",
        color = "Red",
        fontWeight = 'bold'
      )
  })
  
  #tab 4
  
  # Plot data for yearly crime based on UCR parts
  output$ucr_yearly <- renderPlot({
    ggplot(data = ucr_year,
           main = "Crimes based on UCR_Part",
           aes(x = YEAR, y = crime_count, fill = UCR_PART)) +
      geom_col(position = position_dodge()) +
      labs(title = "Boston Crimes Based on UCR PART",
           y = "Number of Crimes Committed",
           x = "Year") + theme_bw(base_size = 10)
    
  })
  # table with yearly UCR crime information 
  output$table_year <- DT::renderDataTable({
    DT::datatable(ucr_year, options = list(orderClasses = TRUE)) %>%
      formatStyle(
        columns = "YEAR",
        target = "cell",
        color = "orange",
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "UCR_PART",
        target = "cell",
        color = "teal",
        fontWeight = 'bold'
      ) %>%
      formatStyle(
        columns = "crime_count",
        target = "cell",
        color = "Red",
        fontWeight = 'bold'
      )
  })
  
  #tab 5
  
  #Time series data plot
  output$ts_year <- renderPlot({
    plot(ts_crimes)
  })
  
  # Seasonal Time series data plot
  output$tsdecom <- renderPlot({
    plot(time_series_decompose)
  })
  
  
  
  }


################################################   Run on Server  ###############################################################

# Run the application
shinyApp(ui = ui, server = server)
