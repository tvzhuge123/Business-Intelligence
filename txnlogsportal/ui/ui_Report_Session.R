tabPanel('Session Report',
  pageWithSidebar(
    headerPanel("Session Report"),
    sidebarPanel(
      dateRangeInput("sessionreport_dateRange", label = "Date range", min = "2018-02-01", max = toString(Sys.Date()), start = toString(Sys.Date()), end = toString(Sys.Date())),
      checkboxInput("sessionreport_excludeWeekend", label = "Exclude weekends data", value = FALSE),
      selectizeInput('sessionreport_sessionName', 'Session type', choices = c("Choose one" = "", 'IccSessions', 'AcctSessions', 'ApptSessions', 'RptSessions', 'LwSessions', 
                                                                                                 'TotalQueries', 'TxnCount', 'MovingAvgQuery', 'AvgQuerySecond', 'FreeMem', 'TotalMem')),
      conditionalPanel(
        condition = "input.sessionreport_sessionName != ''",
        hr(),
        div(style="display:inline-block;", actionButton("sessionreport_run", "Run Analysis", style="color: #fff; background-color: #337ab7; margin-right: 15px;")),
        div(style="display:inline-block;", actionButton("sessionreport_stop", "Stop App", style="color: #fff; background-color: #DB2929;")),
        hr(),
        div(style="display:inline-block; margin-left:0px", downloadButton('sessionreport_downloadData', 'Download Report')),
        br(),
        tags$small(paste0(
          "Note: Run analysis before downloading report with original txnlogs data."
        ))
      ),

      width = input_width
    ),
    
    mainPanel(
      tags$head(tags$style("#table_sessionreport_L3 {white-space: nowrap;}")),
      tags$head(tags$style("#table_sessionreport_SG  {white-space: nowrap;}")),
      tabsetPanel(
        tabPanel('Level 3', DT::dataTableOutput("table_sessionreport_L3")),
        tabPanel('Sungard', DT::dataTableOutput("table_sessionreport_SG"))
      )
    )
    
  )
)