plotsession_outputReady <- reactiveValues(ok = FALSE)

# run event
runAnalysis_plotsession <- eventReactive(input$plotsession_run, {
  shinyjs::disable("plotsession_run")
  plotsession_outputReady$ok <- FALSE
  
  result = list()
  withProgress(message = loading_message_main(), value = 0, {
    n = length(list_dc)
    for (i in 1:n) {
      incProgress(1/(n), detail=loading_message_detail(i))
      tmp = plot_session(input$plotsession_dateRange, input$plotsession_excludeWeekend, input$plotsession_sessionName, list_dc[i])
      result = append(result, list(tmp))
    }
  })
  
  plotsession_outputReady$ok <- TRUE
  return(result)
})

# output
output$plot_plotsession_L3 <- renderPlot({
  if (plotsession_outputReady$ok) {
    shinyjs::enable("plotsession_run")
  }
  validate(need(runAnalysis_plotsession()[[1]][[1]]!=error_var_date_plot, error_date_plot),
           need(runAnalysis_plotsession()[[1]][[1]]!=error_var_noaccess, error_noaccess), 
           need(runAnalysis_plotsession()[[1]][[1]]!=error_var_nodata, error_nodata), 
           need(runAnalysis_plotsession()[[1]][[1]]!=error_var_other, "No data found."))
  runAnalysis_plotsession()[[1]]
}, res = 120, width=1100, height = 600)

output$plot_plotsession_SG <- renderPlot({
  if (plotsession_outputReady$ok) {
    shinyjs::enable("plotsession_run")
  }
  validate(need(runAnalysis_plotsession()[[2]][[1]]!=error_var_date_plot, error_date_plot),
           need(runAnalysis_plotsession()[[2]][[1]]!=error_var_nodata, error_noaccess), 
           need(runAnalysis_plotsession()[[2]][[1]]!=error_var_nodata, error_nodata), 
           need(runAnalysis_plotsession()[[2]][[1]]!=error_var_other, "No data found."))
  runAnalysis_plotsession()[[2]]
}, res = 120, width=1100, height = 600)

# stop app when window closed
session$onSessionEnded(function() {stopApp()})

# stop app button
observeEvent(input$plotsession_stop, {stopApp()})
