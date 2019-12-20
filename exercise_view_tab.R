evTab_UI <- tabItem("ev",
        
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
                         inputId = "exercises", 
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
                     footer = dateRangeInput("date_range", "Select a date range:", start = as.Date('2019-07-01'), end = as.Date('2019-08-01'), min = NULL,
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
                   plotlyOutput("total_weight"),
                   br(), br(), br(), br()
            ),
          ),
          fluidRow(
            column(width = 5,
                   h3("Highest Weight Lifted"),
                   plotlyOutput("weight_time")
            ),
            column(width = 5, offset = 1,
                   h3("Highest Reps Lifted"),
                   plotlyOutput("reps")
            )
          )
        )
)



