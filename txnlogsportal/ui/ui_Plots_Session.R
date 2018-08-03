tabPanel('Sessions',
  pageWithSidebar(
    headerPanel("Plot - Sessions"),
    sidebarPanel(
      dateRangeInput("plotsession_dateRange", label = "Date range", min = "2018-02-01", max = toString(Sys.Date()), start = toString(Sys.Date()), end = toString(Sys.Date())),
      checkboxInput("plotsession_excludeWeekend", label = "Exclude weekends data", value = FALSE),
      selectizeInput('plotsession_sessionName', 'Session type', choices = c("Choose one" = "", 'IccSessions', 'AcctSessions', 'ApptSessions', 'RptSessions', 'LwSessions')),

      conditionalPanel(
        condition = "input.plotsession_sessionName != ''",
        hr(),
        div(style="display:inline-block;", actionButton("plotsession_run", "Generate Plot", style="color: #fff; background-color: #337ab7; margin-right: 15px;")),
        div(style="display:inline-block;", actionButton("plotsession_stop", "Stop App", style="color: #fff; background-color: #DB2929;"))
      ),

      width = input_width
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Level 3', plotOutput("plot_plotsession_L3")),
        tabPanel('Sungard', plotOutput("plot_plotsession_SG"))
      )
    )
  )
)
