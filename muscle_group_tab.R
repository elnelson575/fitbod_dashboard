### UI Function ###

mgTab_UI <- function(id, label = "mg", fitbod_data) {
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      column(width = 6,
             actionButton(ns("savefile"), "Save File", width = '100%'),
             downloadButton(ns("downloadData"), "Download"),
             rHandsontableOutput(ns("muscle"))

      ),
      
      column(width = 6,
             gradientBox(
               # title = "Pick a Date Range:",
               # icon = "fa fa-th",
               # gradientColor = "blue", 
               # boxToolSize = "md", 
               # footer = dateRangeInput(ns("date_range"), "Select a date range:", start = as.Date('2019-07-01'), end = as.Date('2019-08-01'), min = NULL,
               #                         max = NULL, format = "yyyy-mm-dd", startview = "month",
               #                         weekstart = 0, language = "en", separator = " to ", width = NULL,
               #                         autoclose = TRUE),
               #br()
             )
      )
    ),
    fluidRow(
      # column(width = 10,
      #        h3("Total Weight Lifted"),
      #        plotlyOutput(ns("total_weight")),
      #        br(), br(), br(), br()
      #),
    ),
    fluidRow(
      column(width = 5,
             # h3("Highest Weight Lifted"),
             # plotlyOutput(ns("weight_time"))
      ),
      column(width = 5, offset = 1,
             # h3("Highest Reps Lifted"),
             # plotlyOutput(ns("reps"))
      )
    )
  )
  
  #  )
}

### Server Function ###
mgTab_server <- function(input, output, session, fitbod_data, exercises) {
  
  muscle_groupings <- reactive({
    
    exercises$Muscle <- str_trim(exercises$Muscle, side = "left")
    exercises$Exercise <- str_to_title(exercises$Exercise)
    
    combo <- left_join(fitbod_data(), exercises, by = "Exercise") %>%
      select(Exercise, Muscle) %>%
      unique()
    
    combo
  })
  
  output$muscle <- renderRHandsontable({
    rhandsontable(muscle_groupings(), width = 550, height = 300) 
  })

  modified_table <- eventReactive(input$savefile,
               {
                 
                 if (!is.null(isolate(input$muscle))) {
                   #Convert to R object
                   modified_table <- hot_to_r(isolate(input$muscle))
                 }
                 
                 modified_table
               }
  )
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(modified_table(), file)
    }
  )
  
  
}


