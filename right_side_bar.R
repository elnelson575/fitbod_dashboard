
### UI function ###
right_side_bar_UI <- function(id, label = "right_sidebar") {
  ns <- NS(id)
  rightSidebar(
      background = "dark",
      rightSidebarTabContent(
        id = 1,
        icon = "desktop",
        title = "Data Upload",
        active = TRUE,
        fileInput(ns("file1"), "Upload CSV of Fitbod Data",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
      ),
      tags$hr(),
      checkboxInput(ns("include_warmups"), "Include Warm-Up Exercises?", TRUE)
    ),
    rightSidebarTabContent(
      id = 2,
      title = "Tab 2",
      textInput(ns("caption"), "Caption", "Data Summary")
    ),
    rightSidebarTabContent(
      id = 3,
      title = "Tab 3",
      icon = "paint-brush",
      numericInput(ns("obs"), "Observations:", 10, min = 1, max = 100)
    )
  )
}


### Server Function ####


sidebar_server <- function(input, output, session) {
  fitbod_data <- read.csv('fitbod_workout.csv')
  fitbod_data$Date <- as.Date(fitbod_data$Date, format = "%Y-%m-%d")
  clicked <- FALSE
  fitbod_data_updated <- observeEvent(input$file1, {
    clicked <- TRUE
    inFile <- input$file1
    data <- read.csv(inFile$datapath)
    data <- data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
    if (input$include_warmups == TRUE) {
      data <- filter(data, isWarmup != TRUE)
    } 
  })
  
}