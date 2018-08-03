sessionreport_downPar_sessionThreshold <- reactiveValues(value=NULL)
sessionreport_downPar_dateRange <- reactiveValues(value1=NULL, value2=NULL)

sessionreport_outputReady <- reactiveValues(ok = FALSE)
observe({shinyjs::disable("sessionreport_downloadData")})

# run event
runAnalysis_sessionreport <- eventReactive(input$sessionreport_run, {
  sessionreport_downPar_dateRange$value1 <- input$sessionreport_dateRange[1]
  sessionreport_downPar_dateRange$value2 <- input$sessionreport_dateRange[2]
  
  shinyjs::disable("sessionreport_run")
  shinyjs::disable("sessionreport_downloadData")
  sessionreport_outputReady$ok <- FALSE
  
  result = list()
  withProgress(message = loading_message_main(), value = 0, {
    n = length(list_dc)
    for (i in 1:n) {
      incProgress(1/n, detail=loading_message_detail(i))
      tmp = sessionReport_AnalyzeData(input$sessionreport_dateRange, input$sessionreport_excludeWeekend, input$sessionreport_sessionName, list_dc[i])
      result = append(result, list(tmp))
    }
  })

  sessionreport_outputReady$ok <- TRUE
  return(result)
})

# output
output$table_sessionreport_L3 <- DT::renderDataTable({
  if (sessionreport_outputReady$ok) {
    shinyjs::enable("sessionreport_run")
    shinyjs::enable("sessionreport_downloadData")
  }
  validate(need(runAnalysis_sessionreport()[[1]]!=error_var_date, error_date),
           need(runAnalysis_sessionreport()[[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_sessionreport()[[1]]!=error_var_nodata, error_nodata))
  runAnalysis_sessionreport()[[1]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE), rownames=FALSE)

output$table_sessionreport_SG <- DT::renderDataTable({
  if (sessionreport_outputReady$ok) {
    shinyjs::enable("sessionreport_run")
    shinyjs::enable("sessionreport_downloadData")
  }
  validate(need(runAnalysis_sessionreport()[[2]]!=error_var_date, error_date),
           need(runAnalysis_sessionreport()[[2]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_sessionreport()[[2]]!=error_var_nodata, error_nodata))
  runAnalysis_sessionreport()[[2]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE), rownames=FALSE)

output$sessionreport_downloadData <- downloadHandler(
  filename = function() {
    paste('SessionReport_', input$sessionreport_sessionName, '_', sessionreport_downPar_dateRange$value1, 'To', sessionreport_downPar_dateRange$value2, '.xlsx', sep='')
  },
  content = function(file) {
    # write.csv(runAnalysis(), file)
    write.xlsx(runAnalysis_sessionreport()[[1]], file, sheetName="L3 Hourly Average", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_sessionreport()[[2]], file, sheetName="SG Hourly Average", append=TRUE, row.names=FALSE)
  }
)

# stop app when window closed
session$onSessionEnded(session$onSessionEnded(function() {stopApp()}))

# stop app button
observeEvent(input$sessionreport_stop, {stopApp()})