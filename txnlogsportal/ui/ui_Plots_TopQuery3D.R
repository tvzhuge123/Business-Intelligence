tabPanel('Top query - 3D Heat Map',
 pageWithSidebar(
   headerPanel("Plot - Top query - 3D Heat Map"),
   sidebarPanel(
     #helpText("Please choose the input parameters for txnlog plots."),
     selectizeInput('plots3_dataCenter', 'Data center', choices = c('Level 3'='L3', 'Sungard'='SG')),
     selectizeInput('plots3_durationFilter', 'Duration filter (second)', choices = c(0, 100, 1000)),
     checkboxInput("plots3_truncated", label = "Truncated top query (fewer details)", value = TRUE),
     selectizeInput('plots3_rankMode', 'Rank mode', choices = c('Frequency * MeanDuration (MAS)'='MAS', 'Frequency (CNT)'='CNT')),
     dateRangeInput("plots3_dateRange", label = "Date range", min = "2016-10-01", max = toString(Sys.Date()-1), start = toString(Sys.Date()-7), end = toString(Sys.Date()-1)),
     checkboxInput("plots3_excludeWeekend", label = "Exclude weekends data", value = FALSE),
     selectizeInput('plots3_searchMode', 'Search query by', choices = c("Choose one" = "", 'Query rank', 'Type in query manually')),

     conditionalPanel(
       condition = "input.plots3_searchMode == 'Query rank'",
       numericInput("plots3_queryRank", "Query rank (at the last date)", min = 1, max = 100, value = 1, step = 1)
     ),

     conditionalPanel(
       condition = "input.plots3_searchMode == 'Type in query manually'",
       selectizeInput('plots3_queryType', 'Query type', choices = c("Choose one" = "", 'Select', 'Insert', 'Update', 'Delete')),
       textInput("plots3_queryTable",  "Exact table name in the query", value = "")
     ),
     
     conditionalPanel(
       condition = "input.plots3_searchMode != ''",
       hr(),
       div(style="display:inline-block;", actionButton("plots3_run", "Generate Plot", style="color: #fff; background-color: #337ab7; margin-right: 15px;")),
       div(style="display:inline-block;", actionButton("plots3_stop", "Stop App", style="color: #fff; background-color: #DB2929;"))
       # hr(),
       # div(style="display:inline-block; margin-left:0px", downloadButton('plots3_downloadData', 'Download Plot')),
       # br(),
       # tags$small(paste0(
       #   "Note: Run analysis before downloading plot."
       # ))
     ),
     
     width = input_width
   ),
   
   mainPanel(plotOutput("plot_plots3"))
   
 )
)
