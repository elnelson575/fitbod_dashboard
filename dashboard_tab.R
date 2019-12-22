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
             box(uiOutput(ns("dynamic_timeline"))
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
    
  # some variables
  months <- tibble(number = 1:12, name = c("January", "February", "March", "April",
                                           "May", "June", "July", "August", "September",
                                           "October", "November", "December"))
  fitbod_data_months <- unique(month(fitbod_data$Date))
  
  month_names <- filter(months, number %in% fitbod_data_months) %>% select(name)
  
  item_name <- unique(month(fitbod_data$Date))
  item_color <- c("orange", "green", "maroon", "aqua", "purple")
  
  
  val <- data.frame(
      name = NULL,
      time = NULL,
      color = NULL,
      image = NULL,
      stringsAsFactors = FALSE
  )
  
  # add items every 5 seconds 
  # by listening to the random_number
  id = 0
  for (item in month_names) {
    id <- id + 1
    temp_item <- data.frame(
      name = month_names[[id]],
      time = item_name[[id]],
      color = item_color[[id]]
    )
    val <- rbind(val, temp_item)
  }
  
  #generate the dynamic timeline
  output$dynamic_timeline <- renderUI({
    
    items <- val
    len <- nrow(val)
    name <- val$name
    time <- val$time
    color <- val$color
    image <- val$image
    
    #box
    boxPlus(
      width = 6,
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
          style = "height: 400px;",
          timelineStart(color = "danger"),
          br(),
          lapply(1:len, FUN = function(i){
            tagAppendAttributes(
              timelineItem(
                title = name[[i]],
                icon = "medkit",
                color = color[[i]],
                time = dashboardLabel(
                  style = "default",
                  status = "warning",
                  time[[i]]
                ),
                timelineItemMedia(
                  src = image[[i]], 
                  height = 100, 
                  width = 100
                ),
                footer = NULL
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



