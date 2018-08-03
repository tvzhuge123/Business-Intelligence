tabPanel('Client Report',
  pageWithSidebar(
    headerPanel("Client Report"),
    sidebarPanel(
      dateRangeInput("productreport_dateRange", label = "Date range", min = "2017-08-01", max = toString(Sys.Date()-1), start = toString(Sys.Date()-1), end = toString(Sys.Date()-1)),
      checkboxInput("productreport_excludeWeekend", label = "Exclude weekends data", value = FALSE),
      textInput("productreport_accountCode",  "Exact account code", value = ""),

      # sliderInput("productreport_durationFilter", label = "Duration filter (second)",min = 0, max = 240, value = 120, step = 10),
      #selectizeInput('productreport_timeoutThreshold', 'Timeout threshold (second)', choices = c("Choose one" = "", "60"=60000, "120"=120000)),
      hr(),
      div(style="display:inline-block;", actionButton("productreport_run", "Run Analysis", style="color: #fff; background-color: #337ab7; margin-right: 15px;")),
      div(style="display:inline-block;", actionButton("productreport_stop", "Stop App", style="color: #fff; background-color: #DB2929;")),
      hr(),
      div(style="display:inline-block; margin-left:0px", downloadButton('productreport_downloadData', 'Download Report')),
      
      br(),
      tags$small(paste0(
        "Note: Run analysis before downloading report with more detailed data."
      )),
      width = input_width
    ),
    
    mainPanel(
      tags$head(tags$style("#table_productreport1 {white-space: nowrap;}")),
      tags$head(tags$style("#table_productreport2 {white-space: nowrap;}")),
      tabsetPanel(
        tabPanel('Summary', DT::dataTableOutput("table_productreport1")),
        tabPanel('Summary by account', DT::dataTableOutput("table_productreport2"))
      )
    )
    
  )
)
