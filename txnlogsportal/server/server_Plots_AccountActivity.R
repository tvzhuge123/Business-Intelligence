plotsaa_outputReady <- reactiveValues(ok = FALSE)

# run event
runAnalysis_plotsaa <- eventReactive(input$plotsaa_run, {
  shinyjs::disable("plotsaa_run")
  plotsaa_outputReady$ok <- FALSE
  
  withProgress(message = 'Loading...', value = 0, {
    n = 1
    for (i in 1:n) {
      incProgress(1/(n+1), detail=loading_message_detail(i))
      result = plot_accountActivity(input$plotsaa_dateRange, input$plotsaa_excludeWeekend, input$plotsaa_searchMode,
                                 input$plotsaa_accountRank_dataCenter, input$plotsaa_accountRank_database, input$plotsaa_rankMode, input$plotsaa_accountRank_rank,
                                 input$plotsaa_accountCode_database, input$plotsaa_accountCode_code)
    }
  })

  plotsaa_outputReady$ok <- TRUE
  return(result)
})

# output
output$plot_plotsaa <- renderPlot({
  if (plotsaa_outputReady$ok) {
    shinyjs::enable("plotsaa_run")
  }
  validate(need(runAnalysis_plotsaa()[[1]]!=error_var_date_plot, error_date_plot),
           need(runAnalysis_plotsaa()[[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_plotsaa()[[1]]!=error_var_nodata, error_nodata))
  
  runAnalysis_plotsaa()
}, res = 120, width=1000, height = 600)

# stop app when window closed
session$onSessionEnded(function() {stopApp()})

# stop app button
observeEvent(input$plotsaa_stop, {stopApp()})
