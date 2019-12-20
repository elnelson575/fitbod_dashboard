source("exercise_view_tab.R")
source("right_side_bar.R")

primaryUI <- dashboardPagePlus(
  
  header = dashboardHeaderPlus(
    enable_rightsidebar = FALSE,
    rightSidebarIcon = "gears"
  ),
  
  
  sidebar = dashboardSidebar(
    
    
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Muscle Group View", icon = icon("th"), tabName = "widgets", badgeLabel = "new",
               badgeColor = "green"),
      menuItem("Exercise View", icon = icon("bar-chart-o"), tabName = "ev"
      ))
  ),
  
  
  
  body = dashboardBody(
    tabItems(
      tabItem("dashboard",
              div(p("Dashboard tab content"))
      ),
      tabItem("widgets",
              "Widgets tab content"
      ),
      evTab_UI
    )
    
    
  ),
  rightsidebar = right_side_bar_UI,
  title = "FitBod App Monitoring Dashboard"
)