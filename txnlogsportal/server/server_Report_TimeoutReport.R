timeoutreport_downPar_timeoutThreshold <- reactiveValues(value=NULL)
timeoutreport_downPar_dateRange <- reactiveValues(value1=NULL, value2=NULL)

timeoutreport_outputReady <- reactiveValues(ok = FALSE)
observe({shinyjs::disable("timeoutreport_downloadData")})

# run event
runAnalysis_timeoutreport <- eventReactive(input$timeoutreport_run, {
  timeoutreport_downPar_dateRange$value1 <- input$timeoutreport_dateRange[1]
  timeoutreport_downPar_dateRange$value2 <- input$timeoutreport_dateRange[2]
  
  shinyjs::disable("timeoutreport_run")
  shinyjs::disable("timeoutreport_downloadData")
  timeoutreport_outputReady$ok <- FALSE
  
  result = list()
  withProgress(message = loading_message_main(), value = 0, {
    n = length(list_dc)
    for (i in 1:n) {
      incProgress(1/n, detail=loading_message_detail(i))
      tmp = timeoutReport_AnalyzeData(list_dc[i], input$timeoutreport_dateRange, input$timeoutreport_excludeWeekend)
      result = append(result, list(tmp))
    }
  })
  
  timeoutreport_outputReady$ok <- TRUE
  return(result)
})

# output
output$table_timeoutreport_L3_Summary <- DT::renderDataTable({
  if (timeoutreport_outputReady$ok) {
    shinyjs::enable("timeoutreport_run")
    shinyjs::enable("timeoutreport_downloadData")
  }
  validate(need(runAnalysis_timeoutreport()[[1]][[1]]!=error_var_date, error_date),
           need(runAnalysis_timeoutreport()[[1]][[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_timeoutreport()[[1]][[1]]!=error_var_nodata, error_nodata))
  runAnalysis_timeoutreport()[[1]][[1]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = c(0,2,4),
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 40 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)
# }, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, columnDefs=list(list(targets=2:11, class="dt-right"))), rownames=FALSE)

output$table_timeoutreport_L3_Data <- DT::renderDataTable({
  if (timeoutreport_outputReady$ok) {
    shinyjs::enable("timeoutreport_run")
    shinyjs::enable("timeoutreport_downloadData")
  }
  validate(need(runAnalysis_timeoutreport()[[1]][[2]]!=error_var_date, error_date),
           need(runAnalysis_timeoutreport()[[1]][[2]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_timeoutreport()[[1]][[2]]!=error_var_nodata, error_nodata))
  runAnalysis_timeoutreport()[[1]][[2]][,-c(1)]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = c(0,10),
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_timeoutreport_SG_Summary <- DT::renderDataTable({
  if (timeoutreport_outputReady$ok) {
    shinyjs::enable("timeoutreport_run")
    shinyjs::enable("timeoutreport_downloadData")
  }
  validate(need(runAnalysis_timeoutreport()[[2]][[1]]!=error_var_date, error_date),
           need(runAnalysis_timeoutreport()[[2]][[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_timeoutreport()[[2]][[1]]!=error_var_nodata, error_nodata))
  runAnalysis_timeoutreport()[[2]][[1]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = c(0,2,4),
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 40 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 40) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_timeoutreport_SG_Data <- DT::renderDataTable({
  if (timeoutreport_outputReady$ok) {
    shinyjs::enable("timeoutreport_run")
    shinyjs::enable("timeoutreport_downloadData")
  }
  validate(need(runAnalysis_timeoutreport()[[2]][[2]]!=error_var_date, error_date),
           need(runAnalysis_timeoutreport()[[2]][[2]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_timeoutreport()[[2]][[2]]!=error_var_nodata, error_nodata))
  runAnalysis_timeoutreport()[[2]][[2]][,-c(1)]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = c(0,10),
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$timeoutreport_downloadData <- downloadHandler(
  filename = function() {
    paste('ClientReport_', timeoutreport_downPar_dateRange$value1, 'To', timeoutreport_downPar_dateRange$value2, '.xlsx', sep='')
  },
  content = function(file) {
    # write.csv(runAnalysis(), file)
    write.xlsx(runAnalysis_timeoutreport()[[1]][[1]], file, sheetName="L3 Summary", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_timeoutreport()[[1]][[2]], file, sheetName="L3 Data", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_timeoutreport()[[2]][[1]], file, sheetName="SG Summary", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_timeoutreport()[[2]][[2]], file, sheetName="SG Data", append=TRUE, row.names=FALSE)
  }
)

# stop app when window closed
session$onSessionEnded(session$onSessionEnded(function() {stopApp()}))

# stop app button
observeEvent(input$timeoutreport_stop, {stopApp()})