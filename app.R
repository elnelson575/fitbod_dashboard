# Load packages
library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboardPlus)
library(shinydashboard)
library(lubridate)
library(DT)
library(shinyWidgets)
library(rintrojs)
library(stringr)
library(rhandsontable)


# tabs
source("exercise_view_tab.R")
source("right_side_bar.R")
source("dashboard_tab.R")
source("muscle_group_tab.R")

ui <- dashboardPagePlus(
  
  header = dashboardHeaderPlus(
    dropdownBlock(
      id = "helpblock",
      title = "Help!",
      icon = "exclamation",
      introjsUI(),
      actionBttn(
        inputId = "help",
        label = "Tutorial",
        color = "success",
        style = "jelly",
        #icon = icon("exclamation"),
        size = "xs",
        block = FALSE
      )
    ),
    #title = "Fitbod Tracking Dashboard",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears"
  ),
  
  
  sidebar = dashboardSidebar(
    
    
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Muscle Group View", icon = icon("th"), tabName = "mg", badgeLabel = "Coming soon",
               badgeColor = "green"),
      menuItem("Exercise View", icon = icon("bar-chart-o"), tabName = "ev"
      ))
  ),
  
  
  
  body = dashboardBody(
    tabItems(
      tabItem("dashboard", dbTab_UI("dbTab", "Dashboard Tab")
      ),
      tabItem("mg", mgTab_UI("mgTab", "Muscle Group Tab")
      ),
      tabItem("ev", evTab_UI("evTab", "Ev Tab", fitbod_data)
    )
    
    ) 
  ),
  rightsidebar = right_side_bar_UI("right_sidebar", "Right Sidebar"),
  title = "FitBod App Monitoring Dashboard"
)
  

server <- function(input, output, session) {

  startup_modal <- modalDialog(
    title = "Welcome!",
    easyClose = T,
    'Please use the "Help!" button to get oriented, or upload your data using the right sidebar.'
    )
  
  # Show the model on start up ...
  showModal(startup_modal)
  
  
  exercises <- read_delim('Exercises.txt', delim = ",", col_names = FALSE) %>%
    rename(Exercise = X1, Muscle = X2)
  
  help_data <- tibble(step = c(1, 2, 3, 4), 
                      intro = c("Use this menu to upload your FitBod data as a CSV", 
                                "Use this tab to see your dashboard",
                                "Use this tab to see the muscle group view (under development)",
                                "Use this tab to analyze by exercise"),
                      element = c('a[data-toggle*="sidebar"]','a[href*="dashboard"]','a[href*="widgets"]',
                                  'a[href*="ev"]'),
                      position = c("auto", "auto", "auto", "auto"))
  
  observeEvent(input$help, {
    introjs(session, options = list(steps = help_data))
  })
  fitbod_data <- callModule(sidebar_server, "right_sidebar")
  callModule(evTab_server, "evTab", fitbod_data)
  callModule(mgTab_server, "mgTab", fitbod_data, exercises)
  callModule(dbTab_server, "dbTab", fitbod_data)

}

shinyApp(ui, server)