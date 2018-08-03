plots2_outputReady <- reactiveValues(ok = FALSE)

# run event
runAnalysis_plots2 <- eventReactive(input$plots2_run, {
  shinyjs::disable("plots2_run")
  shinyjs::disable("plots2_downloadData")
  plots2_outputReady$ok <- FALSE
  
  result = list()
  withProgress(message = loading_message_main(), value = 0, {
    n = 1
    for (i in 1:n) {
      incProgress(1/(n+1), detail=loading_message_detail(i))
      tmp = plot_topquery_2d(input$plots2_dataCenter, input$plots2_durationFilter, input$plots2_rankMode, 
                             input$plots2_searchMode, input$plots2_queryRank, input$plots2_queryType, input$plots2_queryTable, 
                             input$plots2_plotMode, input$plots2_plotMode_date, input$plots2_plotMode_dateRange, input$plots2_plotMode_excludeWeekend, input$plots2_plotMode_database, input$plots2_truncated)
      result = append(result, list(tmp))
    }
  })
  
  plots2_outputReady$ok <- TRUE
  return(result)
})

# output
output$plot_plots2 <- renderPlot({
  if (plots2_outputReady$ok) {
    shinyjs::enable("plots2_run")
    shinyjs::enable("plots2_downloadData")
  }
  validate(need(runAnalysis_plots2()[[1]]!=error_var_date_plot, error_date_plot),
           need(runAnalysis_plots2()[[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_plots2()[[1]]!=error_var_nodata, error_nodata))
  runAnalysis_plots2()
}, res = 150, width=1100, height = 600)

# output$plots2_db_downloadData <- downloadHandler(
#   filename = function() {
#     paste('TxnlogPlots_TopQuery2D_For', input$plots2_database, '_', input$timeoutreport_dateRange[1], 'To', input$timeoutreport_dateRange[2],'.xlsx', sep='')
#   },
#   content = function(file) {
#     # write.csv(runAnalysis(), file)
#     write.xlsx(runAnalysis_timeoutreport_L3(), file, sheetName="L3", append=TRUE, row.names=FALSE)
#     write.xlsx(runAnalysis_timeoutreport_SG(), file, sheetName="SG", append=TRUE, row.names=FALSE)
#   }
# )

# stop app when window closed
session$onSessionEnded(function() {stopApp()})

# stop app button
observeEvent(input$plots2_stop, {stopApp()})