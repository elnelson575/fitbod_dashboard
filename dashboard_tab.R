### UI Function ###

dbTab_UI <- function(id, label = "ev") {
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      column(width = 5,
        h2("Welcome!")),
      column(width = 5, offset = 3,
             textOutput(ns("latest_data_date")),
             ),
      br(),br(),br()
      ),
    fluidRow(
      column(width = 10,
             h3("Total Weight Lifted"),
             plotlyOutput(ns("total_weight")),
             br(), br(), br(), br()
      ),
    )
  )
  
  #  )
}

### Server Function ###
dbTab_server <- function(input, output, session, fitbod_data) {
  
  output$latest_data_date <- renderPrint(print(paste("Latest Data: ", 
                                        as.character(max(fitbod_data$Date))
                                        )))
}



