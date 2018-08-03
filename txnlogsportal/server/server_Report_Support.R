productreport_downPar_timeoutThreshold <- reactiveValues(value=NULL)
productreport_downPar_dateRange <- reactiveValues(value1=NULL, value2=NULL)

productreport_outputReady <- reactiveValues(ok = FALSE)
observe({shinyjs::disable("productreport_downloadData")})

# run event
runAnalysis_productreport <- eventReactive(input$productreport_run, {
  productreport_downPar_dateRange$value1 <- input$productreport_dateRange[1]
  productreport_downPar_dateRange$value2 <- input$productreport_dateRange[2]
  
  shinyjs::disable("productreport_run")
  shinyjs::disable("productreport_downloadData")
  productreport_outputReady$ok <- FALSE
  
  n = 1
  result = list()
  withProgress(message = loading_message_main(), value = 0, {
    incProgress(1/n, detail=loading_message_detail(i))
    result = productReport_AnalyzeData(input$productreport_dateRange, input$productreport_excludeWeekend, input$productreport_accountCode)
  })
  
  productreport_outputReady$ok <- TRUE
  return(result)
})

# output
output$table_productreport1 <- DT::renderDataTable({
  if (productreport_outputReady$ok) {
    shinyjs::enable("productreport_run")
    shinyjs::enable("productreport_downloadData")
  }
  validate(need(runAnalysis_productreport()[[1]]!=error_var_date, error_date),
           need(runAnalysis_productreport()[[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_productreport()[[1]]!=error_var_nodata, error_nodata))
  runAnalysis_productreport()[[1]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE), rownames=FALSE)

output$table_productreport2 <- DT::renderDataTable({
  if (productreport_outputReady$ok) {
    shinyjs::enable("productreport_run")
    shinyjs::enable("productreport_downloadData")
  }
  validate(need(runAnalysis_productreport()[[2]]!=error_var_date, error_date),
           need(runAnalysis_productreport()[[2]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_productreport()[[2]]!=error_var_nodata, error_nodata))
  runAnalysis_productreport()[[2]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE), rownames=FALSE)

output$productreport_downloadData <- downloadHandler(
  filename = function() {
    paste('ClientTimoutReport_', productreport_downPar_dateRange$value1, 'To', productreport_downPar_dateRange$value2, '.xlsx', sep='')
  },
  content = function(file) {
    write.xlsx(runAnalysis_productreport()[[1]], file, sheetName="Summary", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_productreport()[[2]], file, sheetName="Summary by account", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_productreport()[[3]], file, sheetName="Raw Data", append=TRUE, row.names=FALSE)
  }
)

# stop app when window closed
session$onSessionEnded(session$onSessionEnded(function() {stopApp()}))

# stop app button
observeEvent(input$productreport_stop, {stopApp()})