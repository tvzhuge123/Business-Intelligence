tabPanel('Account Activity',
  pageWithSidebar(
    headerPanel("Account Activity Report"),
    sidebarPanel(
     #selectizeInput('dataCenter', 'Data center', choices = c("Choose one" = "", 'Level 3', 'Sungard')),
     #selectizeInput('accountActivity_dataCenter', 'Data center', choices = c('Level 3'='L3', 'Sungard'='SG')),
     dateRangeInput("accountActivity_dateRange", label = "Date range", min = "2016-10-01", max = toString(Sys.Date()-1), start = toString(Sys.Date()-1), end = toString(Sys.Date()-1)),
     checkboxInput("accountActivity_excludeWeekend", label = "Exclude weekends data", value = FALSE),
     
     hr(),
     div(style="display:inline-block;", actionButton("accountActivity_run", "Run Analysis", style="color: #fff; background-color: #337ab7; margin-right: 15px;")),
     div(style="display:inline-block;", actionButton("accountActivity_stop", "Stop App", style="color: #fff; background-color: #DB2929;")),
     hr(),
     div(style="display:inline-block; margin-left:0px", downloadButton('accountActivity_downloadData', 'Download Report')),
     br(),
     tags$small(paste0(
       "Note: Run analysis before downloading report."
     )),
      width = input_width
    ),
    mainPanel(
     tabsetPanel(
       tabPanel('L3 Overall', DT::dataTableOutput("table_accountActivity_overall_L3")),
       tabPanel('SG Overall', DT::dataTableOutput("table_accountActivity_overall_SG")),
       tabPanel('LInterCAT1', DT::dataTableOutput("table_accountActivity_intercat1_L3")),
       tabPanel('LInterCAT2', DT::dataTableOutput("table_accountActivity_intercat2_L3")),
       tabPanel('LInterCAT3', DT::dataTableOutput("table_accountActivity_intercat3_L3")),
       tabPanel('LInterCAT4', DT::dataTableOutput("table_accountActivity_intercat4_L3")),
       tabPanel('LInterCAT5', DT::dataTableOutput("table_accountActivity_intercat5_L3")),
       tabPanel('LInterCAT6', DT::dataTableOutput("table_accountActivity_intercat6_L3")),
       tabPanel('LInterCAT7', DT::dataTableOutput("table_accountActivity_intercat7_L3")),
       tabPanel('LInterCAT8', DT::dataTableOutput("table_accountActivity_intercat8_L3")),
       tabPanel('LInterCAT9', DT::dataTableOutput("table_accountActivity_intercat9_L3")),
       tabPanel('LInterCAT10', DT::dataTableOutput("table_accountActivity_intercat10_L3")),
       tabPanel('SInterCAT1', DT::dataTableOutput("table_accountActivity_intercat1_SG")),
       tabPanel('SInterCAT2', DT::dataTableOutput("table_accountActivity_intercat2_SG")),
       tabPanel('SInterCAT3', DT::dataTableOutput("table_accountActivity_intercat3_SG")),
       tabPanel('SInterCAT4', DT::dataTableOutput("table_accountActivity_intercat4_SG")),
       tabPanel('SInterCAT5', DT::dataTableOutput("table_accountActivity_intercat5_SG")),
       tabPanel('SInterCAT6', DT::dataTableOutput("table_accountActivity_intercat6_SG")),
       tabPanel('SInterCAT7', DT::dataTableOutput("table_accountActivity_intercat7_SG")),
       tabPanel('SInterCAT8', DT::dataTableOutput("table_accountActivity_intercat8_SG")),
       tabPanel('SInterCAT9', DT::dataTableOutput("table_accountActivity_intercat9_SG")),
       tabPanel('SInterCAT10', DT::dataTableOutput("table_accountActivity_intercat10_SG")),
       tabPanel('PGSInterCAT1', DT::dataTableOutput("table_accountActivity_pgsintercat1")),
       tabPanel('PGSalesDB', DT::dataTableOutput("table_accountActivity_pgsalesdb")),
       tabPanel('SupportDB', DT::dataTableOutput("table_accountActivity_supportdb"))
     )
    )
    )
)
