tabPanel('Timeout Report',
  pageWithSidebar(
    headerPanel("Timeout Report"),
    sidebarPanel(
      dateRangeInput("timeoutreport_dateRange", label = "Date range", min = "2017-08-01", max = toString(Sys.Date()-1), start = toString(Sys.Date()-1), end = toString(Sys.Date()-1)),
      checkboxInput("timeoutreport_excludeWeekend", label = "Exclude weekends data", value = FALSE),

      # sliderInput("timeoutreport_durationFilter", label = "Duration filter (second)",min = 0, max = 240, value = 120, step = 10),
      #selectizeInput('timeoutreport_timeoutThreshold', 'Timeout threshold (second)', choices = c("Choose one" = "", "60"=60000, "120"=120000)),
      hr(),
      div(style="display:inline-block;", actionButton("timeoutreport_run", "Run Analysis", style="color: #fff; background-color: #337ab7; margin-right: 15px;")),
      div(style="display:inline-block;", actionButton("timeoutreport_stop", "Stop App", style="color: #fff; background-color: #DB2929;")),
      hr(),
      div(style="display:inline-block; margin-left:0px", downloadButton('timeoutreport_downloadData', 'Download Report')),
      
      br(),
      tags$small(paste0(
        "Note: Run analysis before downloading report."
      )),
      
     #  tags$script(inactivity),
     #  tags$head(tags$style(type="text/css", "
     # #plots3_loadmessage {
     #   position: fixed;
     #   top: 55px;
     #   left: 0px;
     #   width: 100%;
     #   padding: 20px 0px 20px 0px;
     #   text-align: center;
     #   font-weight: bold;
     #   font-size: 20pt;
     #   color: #000000;
     #   background-color: #00B3AC;
     #   z-index: 105;
     # }")),
     #  conditionalPanel(condition="$('html').hasClass('shiny-busy')", tags$div("Loading...",id="plots3_loadmessage")),
      
      width = input_width
    ),
    
    mainPanel(
      tags$head(tags$style("#table_timeoutreport_L3_Summary {white-space: nowrap;}")),
      tags$head(tags$style("#table_timeoutreport_L3_Data {white-space: nowrap;}")),
      tags$head(tags$style("#table_timeoutreport_SG_Summary {white-space: nowrap;}")),
      tags$head(tags$style("#table_timeoutreport_SG_Data {white-space: nowrap;}")),
      tabsetPanel(
        tabPanel('L3 Summary', DT::dataTableOutput("table_timeoutreport_L3_Summary")),
        tabPanel('L3 Data', DT::dataTableOutput("table_timeoutreport_L3_Data")),
        tabPanel('SG Summary', DT::dataTableOutput("table_timeoutreport_SG_Summary")),
        tabPanel('SG Data', DT::dataTableOutput("table_timeoutreport_SG_Data"))
      )
    )
    
  )
)
