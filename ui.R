
ui <- dashboardPagePlus(
  
  header = dashboardHeaderPlus(
    dropdownBlock(
      id = "helpblock",
      title = "Help!",
      icon = "exclamation",
      introjsUI(),
      actionBttn(
        inputId = "help",
        label = "Tutorial",
        color = "success",
        style = "jelly",
        #icon = icon("exclamation"),
        size = "xs",
        block = FALSE
      )
    ),
    #title = "Fitbod Tracking Dashboard",
    enable_rightsidebar = TRUE,
    rightSidebarIcon = "gears"
  ),
  
  
  sidebar = dashboardSidebar(
    
    
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Muscle Group View", icon = icon("th"), tabName = "mg", badgeLabel = "Coming soon",
               badgeColor = "green"),
      menuItem("Exercise View", icon = icon("bar-chart-o"), tabName = "ev"
      ))
  ),
  
  
  
  body = dashboardBody(
    tabItems(
      tabItem("dashboard", dbTab_UI("dbTab", "Dashboard Tab")
      ),
      tabItem("mg", mgTab_UI("mgTab", "Muscle Group Tab")
      ),
      tabItem("ev", evTab_UI("evTab", "Ev Tab", fitbod_data)
      )
      
    ) 
  ),
  rightsidebar = right_side_bar_UI("right_sidebar", "Right Sidebar"),
  title = "FitBod App Monitoring Dashboard"
)
