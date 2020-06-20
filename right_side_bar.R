
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
      )
    )
    #,
    #rightSidebarTabContent(
      # id = 2,
      # title = "Tab 2",
      # textInput(ns("caption"), "Caption", "Data Summary")
    #),
    #rightSidebarTabContent(
      # id = 3,
      # title = "Tab 3",
      # icon = "paint-brush",
      # numericInput(ns("obs"), "Observations:", 10, min = 1, max = 100)
    #)
  )
}


### Server Function ####


sidebar_server <- function(input, output, session) {
                        
  get_data <- reactive({
    if (is.null(input$file1)) {
      fitbod_data <- read.csv('sample_workout.csv')
    } else {
      read.csv(input$file1$datapath)
    }
  })
  

  dataframe <- reactive({
    data <- get_data()[,c(1:4, 9)]
    colnames(data) <- c("Date", "Exercise", "Reps", "Weight", "isWarmup")
    data$Weight <- data$Weight * 2.20462
    data$Date <- as.Date(data$Date, format = "%Y-%m-%d")
    data <- filter(data, isWarmup != TRUE)
  })
  
  
  return(dataframe)
}