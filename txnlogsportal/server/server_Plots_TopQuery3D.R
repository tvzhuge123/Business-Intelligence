# run event
runAnalysis_plots3 <- eventReactive(input$plots3_run, {
  shinyjs::disable("plots3_run")
  shinyjs::disable("plots3_downloadData")

  result = list()
  withProgress(message = loading_message_main(), value = 0, {
    n = 1
    for (i in 1:n) {
      incProgress(1/(n+1), detail=loading_message_detail(i))
      tmp =   plot_topquery_3d(input$plots3_dataCenter, input$plots3_dateRange, input$plots3_excludeWeekend, "meanduration", input$plots3_durationFilter, input$plots3_rankMode, input$plots3_searchMode, 
                               input$plots3_queryRank, input$plots3_queryType, input$plots3_queryTable, input$plots3_truncated)
      result = append(result, list(tmp))
    }
  })
  
})

# output
output$plot_plots3 <- renderPlot({
  if (is.null(runAnalysis_plots3())) {
    flag = -1
  } else {
    flag = runAnalysis_plots3()
  }
  
  shinyjs::enable("plots3_run")
  shinyjs::enable("plots3_downloadData")
  
  validate(need(flag!=error_var_date_plot, error_date_plot),
           need(flag!=error_var_noaccess, error_noaccess), 
           need(flag!=error_var_nodata, error_nodata),
           need(flag!=error_var_other, "Date range should be longer than 1 day."))
  runAnalysis_plots3()
}, res = 100, width=1100, height = 600)

# output$plots3_downloadData <- downloadHandler(
#   filename = function() {
#     paste("Txnlog3DPlot_", input$plots3_dataCenter, ".png",sep = "")
#   },
#   content = function(file) {
#     png(file,height=800,width=1400,pointsize=18)
#     runAnalysis_plots3()
#     dev.off()
#   }
# )

# stop app when window closed
session$onSessionEnded(function() {stopApp()})

# stop app button
observeEvent(input$plots3_stop, {stopApp()})