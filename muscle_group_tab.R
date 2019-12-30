### UI Function ###

mgTab_UI <- function(id, label = "mg", fitbod_data) {
  ns <- NS(id)
  # tagList(
  fluidPage(
    fluidRow(
      column(width = 6,
             h4("Use this Excel input to check/update the muscle groups of your exercises!"),
             downloadButton(ns("downloadData"), "Download"),
             rHandsontableOutput(ns("muscle")),
             actionButton(ns("savefile"), "Save File and Continue", width = '100%'),
             br(),
             br(),
             fileInput(ns("file2"), "Upload CSV of Muscle Groups:",
                       accept = c(
                         "text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
             )

      ),
      
      column(width = 6,
             gradientBox(
               title = "Pick a Muscle Group:",
               icon = "fa fa-th",
               gradientColor = "blue",
               boxToolSize = "md",
               footer = selectizeInput(
                 inputId = ns("muscle_group"), 
                 label = "Select a muscle group:", 
                 choices = list("glutes", "biceps", "triceps", "upper back", "lower back",
                                "middle back", "neck", "adductors", "quadriceps", "hamstrings",
                                "chest", "abdominals", "shoulders", "calves"), 
                 selected = "biceps",
                 multiple = TRUE
               ),
               br()
             )
      )
    ),
    fluidRow(
      column(width = 10,
             h3("Total Weight Lifted"),
             plotlyOutput(ns("total_weight_mg")),
             br(), br(), br(), br()
      ),
    ),
    fluidRow(
      column(width = 5,
             h3("Highest Weight Lifted"),
             plotlyOutput(ns("weight_time_mg"))
      ),
      column(width = 5, offset = 1,
             h3("Highest Reps Lifted"),
             plotlyOutput(ns("reps_mg"))
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
  
  file2 <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file2, message = FALSE))
    input$file2
  })
  
  modified_table <- eventReactive(c(input$savefile, input$file2), {
                 
                 if (!is.null(isolate(input$muscle))) {
                   #Convert to R object
                   modified_table <- hot_to_r(isolate(input$muscle))
                 }
                 if (!is.null(input$file2)) {
                   user_file <- read.csv(file2()$datapath) %>% select(-X)
                   mod <- rbind(modified_table, user_file)
                   modified_table <- mod
                 }
    modified_table
  })
  
  # modified_table <- eventReactive(input$file2,
  #                                 {
  #                                   user_file <- read.csv(input$file2)
  #                                   mod <- rbind(modified_table(), user_file)
  #                                   View(mod)
  #                                   mod
  #                                 }
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(modified_table(), file)
    }
  )
  
  mg_graph_df <- reactive({
    combo_complete <- left_join(fitbod_data(), modified_table(), by = "Exercise") %>%
      filter(isWarmup == 'false') %>%
      filter(Muscle %in% c(input$muscle_group))
    
    top_weight <- combo_complete %>%
      group_by(Muscle)

    cat <- top_weight %>% top_n(1, Weight) %>% .$Exercise
    top <- top_weight %>% filter(Exercise %in% c(cat))
      
    top
  })
  
  
  total <- reactive({
    total <- mg_graph_df() %>%
      mutate(total_weight_set = Weight * Reps) %>%
      group_by(Exercise, Date) %>%
      summarise(total_weight = sum(total_weight_set)) %>%
      ungroup()
    total
  })
  
  output$total_weight_mg <- renderPlotly({
    plot_ly(total(), x = ~Date, y = ~total_weight, color = ~Exercise) %>%
      add_lines()
  })
  
  output$weight_time_mg <- renderPlotly({
    plot_ly(mg_graph_df(), x = ~Date, y = ~Weight, color = ~Exercise) %>%
      add_lines()
  })
  
  output$reps_mg <- renderPlotly({
    plot_ly(mg_graph_df(), x = ~Date, y = ~Reps, color = ~Exercise) %>%
      add_lines()
  })
  
  
  
  
  
}


