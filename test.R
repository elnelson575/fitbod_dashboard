library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboardPlus)
library(shiny)
library(shinydashboard)
shinyApp(
  ui = dashboardPagePlus(
    header = dashboardHeaderPlus(
      enable_rightsidebar = FALSE,
      rightSidebarIcon = "gears"
    ),
    
    
    sidebar = dashboardSidebar(
      selectizeInput(
      inputId = "exercises", 
      label = "Select an exercise", 
      choices = unique(fitbod_data$Exercise), 
      selected = "Dumbell Bicep Curl",
      multiple = TRUE
    )
    
    ),
    
    
    body = dashboardBody(

      
      
      plotlyOutput("weight_time"),
      plotlyOutput("reps"),
      plotlyOutput("total_weight")
      
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
    title = "Right Sidebar"
  ),
  
  
  
  
  
  server = function(input, output) {
    
    fitbod_data <- read_csv('fitbod_workout.csv')
    data_weights <- fitbod_data %>% filter(Weight > 0)
    total <- fitbod_data %>%
      filter(Weight > 0) %>%
      mutate(total_weight_set = Weight * Reps) %>%
      group_by(Exercise, Date) %>%
      summarise(total_weight = sum(total_weight_set)) %>%
      ungroup()
    
    ## TODO: If no weight, graph reps
    output$total_weight <- renderPlotly({
      plot_ly(total, x = ~Date, y = ~total_weight, color = ~Exercise) %>%
        filter(Exercise %in% input$exercises) %>%
        # group_by(city) %>%
        add_lines()
    })
    
    output$weight_time <- renderPlotly({
      plot_ly(data_weights, x = ~Date, y = ~Weight, color = ~Exercise) %>%
        filter(Exercise %in% input$exercises) %>%
        add_lines()
    })
    
    output$reps <- renderPlotly({
      plot_ly(fitbod_data, x = ~Date, y = ~Reps, color = ~Exercise) %>%
        filter(Exercise %in% input$exercises) %>%
        add_lines()
    })
  }
)
