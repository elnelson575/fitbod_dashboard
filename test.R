library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboardPlus)
library(shiny)
library(shinydashboard)
library(lubridate)



shinyApp(
  
  
  ui = dashboardPagePlus(

    header = dashboardHeaderPlus(
      enable_rightsidebar = FALSE,
      rightSidebarIcon = "gears"
    ),
    
    
    sidebar = dashboardSidebar(
      
      
      sidebarMenu(
        # Setting id makes input$tabs give the tabName of currently-selected tab
        id = "tabs",
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Muscle Group View", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
                 badgeColor = "green"),
        menuItem("Exercise View", icon = icon("bar-chart-o"), tabName = "ev"#,
                 # menuSubItem("Sub-item 1", tabName = "subitem1"),
                 # menuSubItem("Sub-item 2", tabName = "subitem2")
        ))
      ),
      
      
      
    body = dashboardBody(
      tabItems(
        tabItem("dashboard",
                div(p("Dashboard tab content"))
        ),
        tabItem("widgets",
                "Widgets tab content"
        ),
        tabItem("ev",
                
                fluidPage(
                  fluidRow(
                    column(width = 6,
                           gradientBox(
                             title = "Pick an Exercise",
                             icon = "fa fa-th",
                             gradientColor = "teal", 
                             boxToolSize = "md", 
                             footer = 
                               selectizeInput(
                                 inputId = "exercises", 
                                 label = "Select an exercise", 
                                 choices = unique(fitbod_data$Exercise), 
                                 selected = "Dumbell Bicep Curl",
                                 multiple = TRUE
                               ),
                           )
                           ),
                           
                           column(width = 6,
                                  gradientBox(
                                    title = "Pick a Date Range:",
                                    icon = "fa fa-th",
                                    gradientColor = "blue", 
                                    boxToolSize = "md", 
                                    footer = dateRangeInput("date_range", "Select a date range:", start = as.Date('2019-07-01'), end = as.Date('2019-08-01'), min = NULL,
                                                     max = NULL, format = "yyyy-mm-dd", startview = "month",
                                                     weekstart = 0, language = "en", separator = " to ", width = NULL,
                                                     autoclose = TRUE),
                                    br()
                                      )
                                  )
                  ),
                  fluidRow(
                    column(width = 10,
                           h3("Total Weight Lifted"),
                           plotlyOutput("total_weight"),
                           br(), br(), br(), br()
                           ),
                  ),
                  fluidRow(
                    column(width = 5,
                           h3("Highest Weight Lifted"),
                           plotlyOutput("weight_time")
                           ),
                    column(width = 5, offset = 1,
                           h3("Highest Reps Lifted"),
                           plotlyOutput("reps")
                           )
                  )
                )
        )#,
        # tabItem("subitem2",
        #         "Sub-item 2 tab content"
        # )
      )
      
      
    ),
    rightsidebar = rightSidebar(
      background = "dark",
      rightSidebarTabContent(
        id = 1,
        icon = "desktop",
        title = "Tab 1",
        active = TRUE,
        sliderInput(
          "obs", 
          "Number of observations:",
          min = 0, max = 1000, value = 500
        )
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Tab 2",
        textInput("caption", "Caption", "Data Summary")
      ),
      rightSidebarTabContent(
        id = 3,
        title = "Tab 3",
        icon = "paint-brush",
        numericInput("obs", "Observations:", 10, min = 1, max = 100)
      )
    ),
    title = "FitBod App Monitoring Dashboard"
  ),
  
  
  
  
  
  server = function(input, output) {
    ### Import the data
    fitbod_data <- read_csv('fitbod_workout.csv')
    fitbod_data$Date <- as.Date(fitbod_data$Date, format = "%Y-%m-%d")
    
    ### TODO:
    ### Filter for weight over 0 (change so that it shows an error and blank if
    ### there's not weight but there are reps)
    data_weights <- fitbod_data %>% filter(Weight > 0)
    
    total <- fitbod_data %>%
      filter(Weight > 0) %>%
      mutate(total_weight_set = Weight * Reps) %>%
      group_by(Exercise, Date) %>%
      summarise(total_weight = sum(total_weight_set)) %>%
      ungroup()
    

    output$total_weight <- renderPlotly({
      plot_ly(total, x = ~Date, y = ~total_weight, color = ~Exercise) %>%
        filter(Exercise %in% input$exercises) %>%
        filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
        # group_by(city) %>%
        add_lines()
    })
    
    output$weight_time <- renderPlotly({
      plot_ly(data_weights, x = ~Date, y = ~Weight, color = ~Exercise) %>%
        filter(Exercise %in% input$exercises) %>%
        filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
        add_lines()
    })
    
    output$reps <- renderPlotly({
      plot_ly(fitbod_data, x = ~Date, y = ~Reps, color = ~Exercise) %>%
        filter(Exercise %in% input$exercises) %>%
        filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>%
        add_lines()
    })
  }
)
