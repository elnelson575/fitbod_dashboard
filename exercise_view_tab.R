### UI Function ###

evTab_UI <- function(id, label = "ev") {
  ns <- NS(id)
 # tagList(
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
                             inputId = ns("exercises"), 
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
                     footer = dateRangeInput(ns("date_range"), "Select a date range:", start = as.Date('2019-07-01'), end = as.Date('2019-08-01'), min = NULL,
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
                     plotlyOutput(ns("total_weight")),
                     br(), br(), br(), br()
                     ),
              ),
            fluidRow(
              column(width = 5,
                     h3("Highest Weight Lifted"),
                     plotlyOutput(ns("weight_time"))
                     ),
              column(width = 5, offset = 1,
                     h3("Highest Reps Lifted"),
                     plotlyOutput(ns("reps"))
                     )
              )
            )
            
#  )
}

### Server Function ###
evTab_server <- function(input, output, session, fitbod_data) {

  total <- fitbod_data %>%
    filter(Weight > 0) %>%
    mutate(total_weight_set = Weight * Reps) %>%
    group_by(Exercise, Date) %>%
    summarise(total_weight = sum(total_weight_set)) %>%
    ungroup()
  
  output$total_weight <- renderPlotly({
    plot_ly(total, x = ~Date, y = ~total_weight, color = ~Exercise) %>%
      filter(Exercise %in% input$exercises) %>%
      add_lines()
  })
  
  output$weight_time <- renderPlotly({
    plot_ly(fitbod_data, x = ~Date, y = ~Weight, color = ~Exercise) %>%
      filter(Exercise %in% input$exercises) %>%
      add_lines()
  })
  
  output$reps <- renderPlotly({
    plot_ly(fitbod_data, x = ~Date, y = ~Reps, color = ~Exercise) %>%
      filter(Exercise %in% input$exercises) %>%
      # group_by(city) %>%
      add_lines()
  })
}



