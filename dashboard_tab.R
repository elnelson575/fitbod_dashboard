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
               uiOutput(ns("dynamic_timeline"))
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
                                        as.character(max(fitbod_data()$Date))
                                        ))
  total_weight <- reactive({
    total_weight <- fitbod_data() %>%
      filter(Weight > 0) %>%
      mutate(total_weight_set = Weight * Reps) %>%
      summarise(total_weight = sum(total_weight_set)) %>%
      ungroup()
  
    total_weight
  })
  
  output$congrats <- renderText(paste("You've done ", 
                                              as.character(n_distinct(fitbod_data()$Date)),
                                              " workouts and lifted ", 
                                              as.character(total_weight()),
                                              " lbs!"
  ))
  
  
  weights <- tibble(item = c("car", "medium missile", "armored car", "large meteor",
                             "helicopter", "school bus", "space shuttle",
                             "Statue of Liberty"),
                    weight = c(4000, 2600, 25000, 5000, 14000, 14000,
                               220000, 450000)
  )
  
  final <- reactive({
    
    weights_filt <- weights %>%
      filter(weight < as.numeric(total_weight()))

    comp <- weights_filt[sample(1:nrow(weights_filt), 1), ]
  
    num <- total_weight() / comp$weight
  
    final <- paste("You lifted a ", comp$item, " ", round(num,0), " times!")
    
    final
  })
  
  output$comparison <- renderText(paste(final()))
  
  top_exercises <- reactive({
    top_exercises <- fitbod_data() %>%
    select(Exercise, Date) %>%
    group_by(Date) %>%
    unique() %>%
    ungroup() %>%
    count(Exercise) %>%
    arrange(desc(n))
  })
    
  output$top_exercises <- DT::renderDataTable(top_exercises())
    
  # some variables
  months <- tibble(number = 1:12, name = c("January", "February", "March", "April",
                                           "May", "June", "July", "August", "September",
                                           "October", "November", "December"))
  fitbod_data_months <- reactive({ unique(month(fitbod_data()$Date)) })
  
  month_names <- reactive({filter(months, number %in% fitbod_data_months()) %>% select(name) })
  
  item_name <- reactive({
    unique(month(fitbod_data()$Date))
  })
  
  full_list <- reactive({
    
    item_text <- fitbod_data() %>%
      mutate(month = month(Date)) %>%
      group_by(month) %>%
      top_n(1, Weight) %>%
      select(Exercise, month, Weight) %>%
      unique() %>%
      ungroup() %>%
      left_join(months, by = c("month" = "number")) %>%
      rename(time = name)
    
    item_text$footer <- paste("You lifted ", item_text$Weight, " lbs on ", item_text$Exercise)
    full_list <- item_text %>% select(-Exercise, -month, -Weight)
    
    full_list
  })
  
  item_color <- c("orange", "green", "maroon", "aqua", "purple")
  
  
  #generate the dynamic timeline
  output$dynamic_timeline <- renderUI({

    len <- nrow(full_list())
    name <- full_list()$time
    time <- full_list()$time
    color <- item_color
    image <- NULL
    footer <- full_list()$footer
    
    #box
    boxPlus(
      width = 12,
      solidHeader = FALSE,
      status = "primary",
      collapsible = TRUE,
      enable_label = TRUE,
      label_text = len,
      label_status = "danger",
      style = "overflow-y: auto;",
      title = "Recent Events",
      
      # timeline block 
      # only appear when there are timeline items
      if (len > 0) {
        timelineBlock(
          timelineStart(color = "danger"),
          br(),
          lapply(1:len, FUN = function(i){
            tagAppendAttributes(
              timelineItem(
                title = name[[i]],
                icon = "clock-o",
                color = color[[i]],
                time = dashboardLabel(
                  style = "default",
                  status = "warning",
                  time[[i]]
                ),
                footer = footer[[i]]
              ),
              align = "middle"
            )
          }),
          br(),
          timelineEnd(color = "gray")
        )
      }
    )
  })
  
}



