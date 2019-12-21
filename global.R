# Load packages
library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboardPlus)
library(shiny)
library(shinydashboard)
library(lubridate)


# tabs
source("exercise_view_tab.R")
source("right_side_bar.R")
source("dashboard_tab.R")


ui <- dashboardPagePlus(
  
  header = dashboardHeaderPlus(
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears"
  ),
  
  
  sidebar = dashboardSidebar(
    
    
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Muscle Group View", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("Exercise View", icon = icon("bar-chart-o"), tabName = "ev"
      ))
  ),
  
  
  
  body = dashboardBody(
    tabItems(
      tabItem("dashboard", dbTab_UI("dbTab", "Dashboard Tab")
      ),
      tabItem("widgets",
              "Widgets tab content"
      ),
      tabItem("ev", evTab_UI("evTab", "Ev Tab")
    )
    
    ) 
  ),
  rightsidebar = right_side_bar_UI("right_sidebar", "Right Sidebar"),
  title = "FitBod App Monitoring Dashboard"
)
  

server <- function(input, output, session) {
  fitbod_data <- read.csv('fitbod_workout.csv')
  fitbod_data$Date <- as.Date(fitbod_data$Date, format = "%Y-%m-%d")
  callModule(evTab_server, "evTab", fitbod_data)
  callModule(dbTab_server, "dbTab", fitbod_data)
  callModule(sidebar_server, "right_sidebar")
}

shinyApp(ui, server)