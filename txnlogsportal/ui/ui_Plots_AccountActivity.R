tabPanel('Account Activity',
  pageWithSidebar(
    headerPanel("Plot - Account Activity"),
    sidebarPanel(
     #selectizeInput('dataCenter', 'Data center', choices = c("Choose one" = "", 'Level 3', 'Sungard')),
     #selectizeInput('plotsaa_dataCenter', 'Data center', choices = c('Level 3'='L3', 'Sungard'='SG')),
     dateRangeInput("plotsaa_dateRange", label = "Date range", min = "2016-10-01", max = toString(Sys.Date()-1), start = toString(Sys.Date()-1), end = toString(Sys.Date()-1)),
     checkboxInput("plotsaa_excludeWeekend", label = "Exclude weekends data", value = FALSE),
     selectizeInput('plotsaa_searchMode', 'Search account by', choices = c("Choose one" = "", 'Account Code', 'Account Rank')),
     
     conditionalPanel(
       condition = "input.plotsaa_searchMode == 'Account Rank'",
       selectizeInput('plotsaa_accountRank_dataCenter', 'Data center', choices = c('Level 3'='L3', 'Sungard'='SG')),
       selectizeInput('plotsaa_accountRank_database', 'Database', choices = c('Overall', 'InterCAT1', 'InterCAT2', 'InterCAT3', 'InterCAT4', 'InterCAT5', 'InterCAT6', 'InterCAT7', 'InterCAT8', 'InterCAT9', 'InterCAT10')),
       selectizeInput('plotsaa_rankMode', 'Rank account by', choices = c('TotalDuration%'='MAS', 'TxnCount%'='CNT')),
       numericInput("plotsaa_accountRank_rank", "Rank (at the last date)", min = 1, max = 100, value = 1, step = 1)
     ),
     
     conditionalPanel(
       condition = "input.plotsaa_searchMode == 'Account Code'",
       selectizeInput('plotsaa_accountCode_database', 'Database', choices = c('Overall', 'InterCAT')),
       selectizeInput('plotsaa_rankMode', 'Rank account by', choices = c('TotalDuration%'='MAS', 'TxnCount%'='CNT')),
       textInput("plotsaa_accountCode_code",  "Exact account code", value = "")
     ),
     
     conditionalPanel(
       condition = "input.plotsaa_searchMode != ''",
       hr(),
       div(style="display:inline-block;", actionButton("plotsaa_run", "Run Analysis", style="color: #fff; background-color: #337ab7; margin-right: 15px;")),
       div(style="display:inline-block;", actionButton("plotsaa_stop", "Stop App", style="color: #fff; background-color: #DB2929;"))
     ),
     
    width = input_width
    ),
    mainPanel(plotOutput("plot_plotsaa"))
    )
)