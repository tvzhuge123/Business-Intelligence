# source functions if necessary
if(!exists("excludeWeekendFunc", mode = "function")){
  source(file.path("functions", "functions_common.R"), local = TRUE)$value
  source(file.path("functions", "functions_Report_TimeoutReport.R"), local = TRUE)$value
  source(file.path("functions", "functions_Report_TopQuery.R"), local = TRUE)$value
  source(file.path("functions", "functions_Report_AccountActivity.R"), local = TRUE)$value
  source(file.path("functions", "functions_Report_Support.R"), local = TRUE)$value
  source(file.path("functions", "functions_Report_Session.R"), local = TRUE)$value
  source(file.path("functions", "functions_Plots_TopQuery2D.R"), local = TRUE)$value
  source(file.path("functions", "functions_Plots_TopQuery3D.R"), local = TRUE)$value
  source(file.path("functions", "functions_Plots_AccountActivity.R"), local = TRUE)$value
  source(file.path("functions", "functions_Plots_Session.R"), local = TRUE)$value
}

# load libraries if necessary
if(!"package:RPostgreSQL" %in% search()){loadLibrary()}

# ui
ui <- navbarPage(
  "Txnlogs Portal",
  id = "navbar",
  
  # include the UI for each tab
  source(file.path("ui", "ui_Report_TimeoutReport.R"), local = TRUE)$value,
  source(file.path("ui", "ui_Report_Support.R"), local = TRUE)$value,
  source(file.path("ui", "ui_Report_TopQuery.R"), local = TRUE)$value,
  source(file.path("ui", "ui_Report_Session.R"), local = TRUE)$value,
  source(file.path("ui", "ui_Report_AccountActivity.R"), local = TRUE)$value,

  navbarMenu("Plots",
    source(file.path("ui", "ui_Plots_Session.R"),  local = TRUE)$value, 
    source(file.path("ui", "ui_Plots_TopQuery2D.R"), local = TRUE)$value,
    source(file.path("ui", "ui_Plots_TopQuery3D.R"), local = TRUE)$value,
    source(file.path("ui", "ui_Plots_AccountActivity.R"),  local = TRUE)$value
  )
)

# server
server <- function(input, output, session) {
  # include the logic (server) for each tab
  source(file.path("server", "server_Report_TimeoutReport.R"), local = TRUE)$value
  source(file.path("server", "server_Report_TopQuery.R"), local = TRUE)$value
  source(file.path("server", "server_Report_AccountActivity.R"), local = TRUE)$value
  source(file.path("server", "server_Report_Support.R"), local = TRUE)$value
  source(file.path("server", "server_Report_Session.R"), local = TRUE)$value
  source(file.path("server", "server_Plots_TopQuery2D.R"), local = TRUE)$value
  source(file.path("server", "server_Plots_TopQuery3D.R"), local = TRUE)$value
  source(file.path("server", "server_Plots_AccountActivity.R"), local = TRUE)$value
  source(file.path("server", "server_Plots_Session.R"), local = TRUE)$value
}

shinyApp(ui, server)