### UI Function ###

dbTab_UI <- function(id, label = "ev") {
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      column(width = 5,
        h2("Welcome!")),
      column(width = 5, offset = 9,
             textOutput(ns("latest_data_date")),
             ),
      br(),br(),br()
      ),
    fluidRow(
      column(width = 10,
             h3(textOutput(ns("congrats"))),
             br(), br(), br(), br()
      ),
    )
  )
  
  #  )
}

### Server Function ###
dbTab_server <- function(input, output, session, fitbod_data) {
  
  output$latest_data_date <- renderText(paste("Latest Data: ", 
                                        as.character(max(fitbod_data$Date))
                                        ))
  total_weight <- fitbod_data %>%
    filter(Weight > 0) %>%
    mutate(total_weight_set = Weight * Reps) %>%
    summarise(total_weight = sum(total_weight_set)) %>%
    ungroup()
  
  output$congrats <- renderText(paste("You've done ", 
                                              as.character(n_distinct(fitbod_data$Date)),
                                              " workouts and lifted ", 
                                              as.character(total_weight),
                                              " lbs!"
  ))
}



