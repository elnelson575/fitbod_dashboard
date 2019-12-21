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
             h2(textOutput(ns("comparison"))),
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
  
  
  weights <- tibble(item = c("car", "medium missile", "armored car", "large meteor",
                             "helicopter", "school bus", "space shuttle",
                             "Statue of Liberty"),
                    weight = c(4000, 2600, 25000, 5000, 14000, 14000,
                               220000, 450000)
  )
  
  weights_filt <- filter(weights, weight <= total_weight)
  comp <- weights_filt[sample(1:nrow(weights_filt), 1), ]
  
  num <- total_weight / comp$weight
  
  final <- paste("You lifted a ", comp$item, " ", round(num,0), " times!")
  
  
  
  output$comparison <- renderText(paste(final))
}



