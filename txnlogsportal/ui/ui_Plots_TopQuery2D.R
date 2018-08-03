tabPanel('Top query - 2D Boxplot',
 pageWithSidebar(
   headerPanel("Plot - Top query - 2D Boxplot"),
   sidebarPanel(
     #helpText("Please choose the input parameters for txnlog plots."),
     selectizeInput('plots2_plotMode', 'Plot query duration against', choices = c("Choose one" = "", 'Dates', 'Databases')),
     
     conditionalPanel(
       condition = "input.plots2_plotMode == 'Databases'",
       dateInput("plots2_plotMode_date", label = "Date", min = "2016-10-01", max = toString(Sys.Date()-1), value = toString(Sys.Date()-1))
     ),
     
     conditionalPanel(
       condition = "input.plots2_plotMode == 'Dates'",
       dateRangeInput("plots2_plotMode_dateRange", label = "Date range", min = "2016-10-01", max = toString(Sys.Date()-1), start = toString(Sys.Date()-1), end = toString(Sys.Date()-1)),
       checkboxInput("plots2_plotMode_excludeWeekend", label = "Exclude weekends data", value = FALSE),
       selectizeInput('plots2_plotMode_database', 'Database', choices = c('Overall', 'InterCAT1', 'InterCAT2', 'InterCAT3', 'InterCAT4', 'InterCAT5', 'InterCAT6', 'InterCAT7', 'InterCAT8', 'InterCAT9', 'InterCAT10'))
     ),
     
     conditionalPanel(
       condition = "input.plots2_plotMode != ''",
        checkboxInput("plots2_truncated", label = "Truncated top query (fewer details)", value = TRUE),
        selectizeInput('plots2_dataCenter', 'Data center', choices = c('Level 3'='L3', 'Sungard'='SG')),
        selectizeInput('plots2_durationFilter', 'Duration filter (second)', choices = c(0, 100, 1000)),
        selectizeInput('plots2_rankMode', 'Rank query by', choices = c('Frequency * MeanDuration (MAS)'='MAS', 'Frequency (CNT)'='CNT')),
        selectizeInput('plots2_searchMode', 'Search query by', choices = c("Choose one" = "", 'Query rank', 'Type in query manually'))
     ),
     
     conditionalPanel(
       condition = "input.plots2_searchMode == 'Query rank'",
       numericInput("plots2_queryRank", "Query rank (at the last date)", min = 1, max = 100, value = 1, step = 1)
     ),
     
     conditionalPanel(
       condition = "input.plots2_searchMode == 'Type in query manually'",
       selectizeInput('plots2_queryType', 'Query type', choices = c("Choose one" = "", 'Select', 'Insert', 'Update', 'Delete')),
       textInput("plots2_queryTable",  "Exact table name in the query", value = "")
     ),
     
     conditionalPanel(
       condition = "input.plots2_searchMode != ''",
       hr(),
       div(style="display:inline-block;", actionButton("plots2_run", "Generate Plot", style="color: #fff; background-color: #337ab7; margin-right: 15px;")),
       div(style="display:inline-block;", actionButton("plots2_stop", "Stop App", style="color: #fff; background-color: #DB2929;"))
       # hr(),
       # div(style="display:inline-block; margin-left:0px", downloadButton('plots2_downloadData', 'Download Plot')),
       # br(),
       # tags$small(paste0(
       #   "Note: Run analysis before downloading plot."
       # ))
     ),
     
     width = input_width
   ),
   
   mainPanel(
     plotOutput("plot_plots2")
   )
   
 )
)
