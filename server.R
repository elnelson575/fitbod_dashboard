
server <- function(input, output, session) {
  
  startup_modal <- modalDialog(
    title = "Welcome!",
    easyClose = T,
    'Please use the "Help!" button to get oriented, or upload your data using the right sidebar.
    The app will start with sample data in place to show you around if you do not have data of your own.'
  )
  
  # Show the model on start up ...
  showModal(startup_modal)
  
  exercises <- read_delim('Exercises.txt', delim = ",", col_names = FALSE) %>%
    rename(Exercise = X1, Muscle = X2)
  
  help_data <- tibble(step = c(1, 2, 3, 4), 
                      intro = c("Use this menu to upload your FitBod data as a CSV", 
                                "Use this tab to see your dashboard",
                                "Use this tab to see the muscle group view (under development)",
                                "Use this tab to analyze by exercise"),
                      element = c('a[data-toggle*="sidebar"]','a[href*="dashboard"]','a[href*="widgets"]',
                                  'a[href*="ev"]'),
                      position = c("auto", "auto", "auto", "auto"))
  
  observeEvent(input$help, {
    introjs(session, options = list(steps = help_data))
  })
  fitbod_data <- callModule(sidebar_server, "right_sidebar", backup_data)
  callModule(evTab_server, "evTab", fitbod_data)
  #callModule(mgTab_server, "mgTab", fitbod_data, exercises)
  callModule(dbTab_server, "dbTab", fitbod_data)
  
}