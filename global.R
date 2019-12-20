# Load packages
library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboardPlus)
library(shiny)
library(shinydashboard)
library(lubridate)


# tabs
source("gen_UI.R")
source("exercise_view_tab.R")
source("right_side_bar.R")



shinyApp(
  
  
  ui = primaryUI,
  
  
  
  
  
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
