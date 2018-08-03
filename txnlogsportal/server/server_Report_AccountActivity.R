accountActivity_outputReady <- reactiveValues(ok = FALSE)
observe({shinyjs::disable("accountActivity_downloadData")})

# run event
runAnalysis_accountActivity <- eventReactive(input$accountActivity_run, {
  shinyjs::disable("accountActivity_run")
  shinyjs::disable("accountActivity_downloadData")
  accountActivity_outputReady$ok <- FALSE
  
  result = list()
  withProgress(message = loading_message_main(), value = 0, {
    n = length(list_dc)
    m = length(list_db)
    count = 1
    for (i in 1:n) {
      if(list_dc[i]=="SG"){list_db = c(list_db, list_db_pg)}
      result2 = list()
      for (j in 1:length(list_db)){
        incProgress(1/(m*n+length(list_db_pg)), detail=loading_message_detail(count))
        count = count + 1
        tmp = accountActivity_AnalyzeData(list_dc[i], list_db[j], input$accountActivity_dateRange, input$accountActivity_excludeWeekend)
        result2 = append(result2, list(tmp))
      }
      result = append(result, list(result2))
    }
  })
  
  accountActivity_outputReady$ok <- TRUE
  return(result)
})

# output table
output$table_accountActivity_intercat1_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[1]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[1]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[1]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right")))
, rownames=FALSE)


output$table_accountActivity_intercat2_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[2]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[2]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[2]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[2]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right")))
, rownames=FALSE)

output$table_accountActivity_intercat3_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[3]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[3]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[3]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[3]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right")))
, rownames=FALSE)

output$table_accountActivity_intercat4_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[4]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[4]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[4]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[4]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right")))
, rownames=FALSE)

output$table_accountActivity_intercat5_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[5]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[5]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[5]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[5]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat6_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[6]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[6]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[6]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[6]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat7_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[7]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[7]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[7]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[7]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat8_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[8]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[8]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[8]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[8]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat9_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[9]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[9]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[9]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[9]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat10_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[10]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[10]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[10]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[10]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_overall_L3 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[1]][[11]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[1]][[11]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[1]][[11]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[1]][[11]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat1_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[1]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[1]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[1]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat2_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[2]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[2]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[2]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[2]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat3_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[3]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[3]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[3]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[3]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat4_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[4]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[4]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[4]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[4]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat5_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[5]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[5]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[5]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[5]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat6_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[6]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[6]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[6]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[6]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat7_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[7]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[7]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[7]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[7]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat8_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[8]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[8]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[8]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[8]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat9_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[9]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[9]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[9]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[9]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_intercat10_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[10]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[10]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[10]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[10]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_overall_SG <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[11]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[11]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[11]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[11]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_pgsintercat1 <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[12]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[12]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[12]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[12]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_pgsalesdb <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[13]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[13]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[13]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[13]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

output$table_accountActivity_supportdb <- DT::renderDataTable({
  if (accountActivity_outputReady$ok) {
    shinyjs::enable("accountActivity_run")
    shinyjs::enable("accountActivity_downloadData")
  }
  validate(need(runAnalysis_accountActivity()[[2]][[14]]!=error_var_date, error_date),
           need(runAnalysis_accountActivity()[[2]][[14]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_accountActivity()[[2]][[14]]!=error_var_nodata, error_nodata))
  runAnalysis_accountActivity()[[2]][[14]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, columnDefs=list(list(targets=2:5, class="dt-right"))) , rownames=FALSE)

# download
output$accountActivity_downloadData <- downloadHandler(
  filename = function() {
    paste('TxnlogsAccountActivityReport_', input$accountActivity_dataCenter, '_', input$accountActivity_dateRange[1], 'To', input$accountActivity_dateRange[2],'.xlsx', sep='')
  },
  content = function(file) {
    # write.csv(runAnalysis(), file)
    write.xlsx(runAnalysis_accountActivity()[[1]][[11]], file, sheetName="L3 Overall", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[11]], file, sheetName="SG Overall", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[1]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT1', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[2]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT2', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[3]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT3', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[4]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT4', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[5]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT5', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[6]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT6', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[7]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT7', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[8]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT8', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[9]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT9', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[1]][[10]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'LInterCAT10', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[1]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT1', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[2]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT2', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[3]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT3', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[4]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT4', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[5]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT5', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[6]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT6', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[7]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT7', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[8]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT8', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[9]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT9', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[10]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SInterCAT10', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[12]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'PGSInterCAT1', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[13]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'PGSalesDB', sep=''), append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_accountActivity()[[2]][[14]], file, sheetName=paste(tolower(substr(input$accountActivity_dataCenter,1,1)), 'SupportDB', sep=''), append=TRUE, row.names=FALSE)
  }
)

# stop app when window closed
session$onSessionEnded(function() {stopApp()})

# stop app button
observeEvent(input$accountActivity_stop, {stopApp()})