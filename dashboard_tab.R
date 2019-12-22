### UI Function ###

dbTab_UI <- function(id, label = "dashboard") {
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
    ),
    fluidRow(
      column(width = 6,
             box(
               title = "Timeline",
               status = "info",
               timelineBlock(
                 timelineEnd(color = "danger"),
                 timelineLabel(2018, color = "teal"),
                 timelineItem(
                   title = "Item 1",
                   icon = "gears",
                   color = "olive",
                   time = "now",
                   footer = "Here is the footer",
                   "This is the body"
                 ),
                 timelineItem(
                   title = "Item 2",
                   border = FALSE
                 ),
                 timelineLabel(2015, color = "orange"),
                 timelineItem(
                   title = "Item 3",
                   icon = "paint-brush",
                   color = "maroon",
                   timelineItemMedia(src = "http://placehold.it/150x100"),
                   timelineItemMedia(src = "http://placehold.it/150x100")
                 ),
                 timelineStart(color = "gray")
               )
             ),
             
      ),
      column(width = 6,
             gradientBox(
               title = "Top Exercises",
               icon = "fa fa-th",
               gradientColor = "teal", 
               boxToolSize = "lg", 
               footer = DT::dataTableOutput(ns("top_exercises"))
             )
      )
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
  
  
  weights_filt <- weights %>%
    filter(weight < as.numeric(total_weight))

  comp <- weights_filt[sample(1:nrow(weights_filt), 1), ]
  
  num <- total_weight / comp$weight
  
  final <- paste("You lifted a ", comp$item, " ", round(num,0), " times!")
  
  output$comparison <- renderText(paste(final))
  
  top_exercises <- fitbod_data %>%
    select(Exercise, Date) %>%
    group_by(Date) %>%
    unique() %>%
    ungroup() %>%
    count(Exercise) %>%
    arrange(desc(n))
    
  output$top_exercises <- DT::renderDataTable(top_exercises)
    
}



