
library(shiny)
library(tidyverse)
library(plotly)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Weight Over Time"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput(
                inputId = "exercises", 
                label = "Select an exercise", 
                choices = unique(fitbod_data$Exercise), 
                selected = "Dumbell Bicep Curl",
                multiple = TRUE
                )
            ),

        # Show a plot of the generated distribution
        mainPanel(
           plotlyOutput("weight_time"),
           plotlyOutput("reps"),
           plotlyOutput("total_weight")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    fitbod_data <- read_csv('fitbod_workout.csv')
    data_weights <- fitbod_data %>% filter(Weight > 0)
    total <- fitbod_data %>%
        filter(Weight > 0) %>%
        mutate(total_weight_set = Weight * Reps) %>%
        group_by(Exercise, Date) %>%
        summarise(total_weight = sum(total_weight_set)) %>%
        ungroup()
    
    output$total_weight <- renderPlotly({
        plot_ly(total, x = ~Date, y = ~total_weight, color = ~Exercise) %>%
            filter(Exercise %in% input$exercises) %>%
            # group_by(city) %>%
            add_lines()
    })

    output$weight_time <- renderPlotly({
        plot_ly(data_weights, x = ~Date, y = ~Weight, color = ~Exercise) %>%
            filter(Exercise %in% input$exercises) %>%
           # group_by(city) %>%
            add_lines()
    })
    
    output$reps <- renderPlotly({
        plot_ly(fitbod_data, x = ~Date, y = ~Reps, color = ~Exercise) %>%
            filter(Exercise %in% input$exercises) %>%
            # group_by(city) %>%
            add_lines()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
