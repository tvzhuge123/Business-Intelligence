topquery_outputReady <- reactiveValues(ok = FALSE)
observe({shinyjs::disable("topquery_downloadData")})

# run event
runAnalysis_topquery <- eventReactive(input$topquery_run, {
  shinyjs::disable("topquery_run")
  shinyjs::disable("topquery_downloadData")
  topquery_outputReady$ok <- FALSE
  
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
        tmp = topQuery_AnalyzeData(list_dc[i], list_db[j], input$topquery_dateRange, input$topquery_excludeWeekend, input$topquery_truncated, input$topquery_durationFilter, input$topquery_rankMode, input$topquery_rankRange)
        result2 = append(result2, list(tmp))
      }
      result = append(result, list(result2))
    }
  })
  
  topquery_outputReady$ok <- TRUE
  return(result)
})

# output table
output$table_topquery_loverall <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[11]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[11]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[11]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[11]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)
# }, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, columnDefs=list(list(targets=3:10, class="dt-right"))), rownames=FALSE)

output$table_topquery_lintercat1 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[1]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[1]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[1]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat2 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[2]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[2]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[2]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[2]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat3 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[3]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[3]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[3]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[3]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat4 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[4]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[4]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[4]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[4]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat5 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[5]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[5]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[5]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[5]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat6 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[6]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[6]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[6]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[6]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat7 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[7]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[7]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[7]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[7]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat8 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[8]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[8]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[8]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[8]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat9 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[9]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[9]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[9]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[9]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_lintercat10 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[1]][[10]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[1]][[10]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[1]][[10]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[1]][[10]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat1 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[1]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[1]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[1]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[1]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat2 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[2]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[2]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[2]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[2]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat3 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[3]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[3]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[3]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[3]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat4 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[4]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[4]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[4]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[4]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat5 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[5]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[5]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[5]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[5]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat6 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[6]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[6]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[6]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[6]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat7 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[7]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[7]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[7]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[7]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat8 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[8]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[8]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[8]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[8]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat9 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[9]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[9]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[9]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[9]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_sintercat10 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[10]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[10]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[10]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[10]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_soverall <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[11]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[11]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[11]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[11]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_pgsintercat1 <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[12]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[12]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[12]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[12]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_pgsalesdb <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[13]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[13]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[13]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[13]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

output$table_topquery_supportdb <- DT::renderDataTable({
  if (topquery_outputReady$ok) {
    shinyjs::enable("topquery_run")
    shinyjs::enable("topquery_downloadData")
  }
  validate(need(runAnalysis_topquery()[[2]][[14]]!=error_var_date, error_date),
           need(runAnalysis_topquery()[[2]][[14]]!=error_var_noaccess, error_noaccess),
           need(runAnalysis_topquery()[[2]][[14]]!=error_var_nodata, error_nodata))
  runAnalysis_topquery()[[2]][[14]]
}, options = list(lengthMenu = c(15, 25, 50), pageLength = 15, scrollX=TRUE, scrollY=TRUE, 
                  columnDefs = list(list(
                    targets = 0,
                    render = JS(
                      "function(data, type, row, meta) {",
                      "return type === 'display' && data.length > 50 ?",
                      "'<span title=\"' + data + '\">' + data.substr(0, 50) + '...</span>' : data;",
                      "}")
                  ))), rownames=FALSE)

# download
output$topquery_downloadData <- downloadHandler(
  filename = function() {
    paste('TxnlogsTopQueryReport_', input$topquery_rankMode, '_', input$topquery_dateRange[1], 'To', input$topquery_dateRange[2],'.xlsx', sep='')
  },
  content = function(file) {
    # write.csv(runAnalysis(), file)
    write.xlsx(runAnalysis_topquery()[[1]][[11]], file, sheetName="L3 All DBs", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[11]], file, sheetName="SG All DBs", append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[1]], file, sheetName='LInterCAT1', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[2]], file, sheetName='LInterCAT2', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[3]], file, sheetName='LInterCAT3', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[4]], file, sheetName='LInterCAT4', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[5]], file, sheetName='LInterCAT5', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[6]], file, sheetName='LInterCAT6', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[7]], file, sheetName='LInterCAT7', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[8]], file, sheetName='LInterCAT8', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[9]], file, sheetName='LInterCAT9', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[1]][[10]], file, sheetName='LInterCAT10', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[1]], file, sheetName='SInterCAT1', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[2]], file, sheetName='SInterCAT2', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[3]], file, sheetName='SInterCAT3', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[4]], file, sheetName='SInterCAT4', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[5]], file, sheetName='SInterCAT5', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[6]], file, sheetName='SInterCAT6', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[7]], file, sheetName='SInterCAT7', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[8]], file, sheetName='SInterCAT8', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[9]], file, sheetName='SInterCAT9', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[10]], file, sheetName='SInterCAT10', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[12]], file, sheetName='PGSInterCAT1', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[13]], file, sheetName='PGSalesDB', append=TRUE, row.names=FALSE)
    write.xlsx(runAnalysis_topquery()[[2]][[14]], file, sheetName='SupportDB', append=TRUE, row.names=FALSE)
  }
)

# stop app when window closed
session$onSessionEnded(session$onSessionEnded(function() {stopApp()}))

# stop app button
observeEvent(input$topquery_stop, {stopApp()})