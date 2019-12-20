right_side_bar_UI <- rightSidebar(
  background = "dark",
  rightSidebarTabContent(
    id = 1,
    icon = "desktop",
    title = "Data Upload",
    active = TRUE,
    fileInput("file1", "Upload CSV of Fitbod Data",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),
    tags$hr(),
    checkboxInput("include_warmups", "Include Warm-Up Exercises?", TRUE)
  ),
  rightSidebarTabContent(
    id = 2,
    title = "Tab 2",
    textInput("caption", "Caption", "Data Summary")
  ),
  rightSidebarTabContent(
    id = 3,
    title = "Tab 3",
    icon = "paint-brush",
    numericInput("obs", "Observations:", 10, min = 1, max = 100)
  )
)
