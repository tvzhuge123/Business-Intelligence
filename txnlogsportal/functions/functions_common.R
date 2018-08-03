# system('shiny-server --version', intern = TRUE)
# .rs.restartR()

# load library
loadLibrary = function(){
  lib_required = c("methods", "RODBC", "DBI", "RPostgreSQL", "plyr", "xlsx", "digest", "reshape2", "lubridate", "tictoc",
                   "ggplot2", "plotly", "gridExtra", "scales", "plot3D", "scatterplot3d", "shiny", "shinyjs", "DT")
  lib_available = (.packages(all.available=TRUE))
  for(lib in lib_required){
    if(!lib%in%lib_available){install.packages(lib, dependencies=TRUE, repos='http://cran.rstudio.com/')}
    library(lib, character.only=TRUE, quietly=TRUE, warn.conflicts = FALSE)
  }
}

# update library
updateLibrary = function(){
  update.packages(ask = FALSE, checkBuilt = TRUE, repos='http://cran.rstudio.com/')
}

# list
list_dc = c("L3", "SG")
list_db = c('intercat1', 'intercat2', 'intercat3', 'intercat4', 'intercat5', 'intercat6', 'intercat7', 'intercat8', 'intercat9', 'intercat10', 'overall')
list_db_pg = c('pgsintercat1', 'pgsalesdb', 'supportdb')

# error pars and messages
dataRangeLimit <- 100
dataRangeLimit_plot <- 30

error_date <- paste("Selected date range exceeds ", dataRangeLimit, " day limit.", sep="")
error_date_plot <- paste("Selected date range exceeds ", dataRangeLimit_plot, " day limit.", sep="")
error_noaccess <- "Access to data center is denied, please contact administrator."
error_nodata <- "No data found, please modify your input parameters."

error_var_date = "error_var_date"
error_var_date_plot = "error_var_date_plot"
error_var_noaccess = "error_noaccess"
error_var_nodata = "error_nodata"
error_var_other = "error_var_other"

input_width = 3

loading_message_main = function(){
  return("Loading...")
}

loading_message_detail = function(i){
  # return(paste(i,"%",sep=""))
  return("")
}

# idle session timeout
inactivity <- "
function idleTimer() {
  var t = setTimeout(logout, 5000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions
  
  function logout() {
    window.close();  //close the window
  }
  
  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 5000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();
"

# load data from Postgres
loadLogData = function(query, dataCenter)
{
  # make connection and fetch data
  drv = dbDriver('PostgreSQL')
  drvname = drvnamefunc(dataCenter)
  connection = tryCatch({
    dbConnect(drv, host = drvname[1], port='5432', dbname=drvname[2], user = "", password = "")
  }, error = function(e) {
    -9999
  })
  if(is.numeric(connection)){
    return(-9999)
  }
  
  stmt = dbSendQuery(connection, query)
  dataframe = dbFetch(stmt, n=-1)
  
  # disconnect server
  for(con in dbListConnections(drv)) dbDisconnect(con) 
  
  return(dataframe)
}

# load data from SQL Server
loadDataSQLServer = function(query)
{
  mydsn = "dbserverdsn"
  channel = tryCatch({
    odbcConnect(mydsn, "", "")
  }, warning = function(e) {
    -9999
  })
  if(channel==-9999){
    return(-9999)
  }
  dataframe = sqlQuery(channel, query)
  odbcClose(channel)
  
  return(dataframe)
}

# stop query
stopShiny = function(query){
  stopApp()
}

# exclude weekend data
excludeWeekendFunc = function(data){
  if(nrow(data)!=0){
    data$mydate = format(as.Date(as.character(data$dts)), format="%b-%d %a")
    data = data[which(!grepl(paste(c("Sat","Sun"), collapse = "|"), data$mydate)),]
    data = subset(data, select=-c(mydate))
  }
  return(data)
}

# cumstomized sd function
mysd = function(data){
  data = data[which(!is.na(data))]
  if(length(data)!=0){
    return(mean(data))
  } else{
    return(NA)
  }
}

# dc name
drvnamefunc = function(dataCenter)
{
  if(dataCenter=='L3'){
    dbip = ""
  }else if(dataCenter=='SG'){
    dbip = ""
  }
  
  dbname = paste(tolower(substr(dataCenter,1,1)), "transactionlogs", sep="")
  
  return(c(dbip, dbname))
}

# parse data center name
dataCenterParse = function(dataCenter){
  if(dataCenter=="Level 3"){
    dataCenter = "L3"
  }
  else if(dataCenter=="Sungard"){
    dataCenter = "SG"
  }
  return(dataCenter)
}

# parse rank mode name
rankModeParse = function(rankMode){
  if(rankMode=="Frequency"){
    rankMode = "CNT"
  } else if(rankMode=="Mass (Frequency * MeanDuration)"){
    rankMode = "MAS"
  }
  return(rankMode)
}

# parse database name
databaseNameParse = function(dataCenter, databaseName){
  databaseName = tolower(databaseName)
  if(!databaseName %in% list_db_pg){
    if(databaseName=='overall'){
      databaseName = 'all'
    }
    # else if(startsWith(databaseName, 'pg')){
    #   databaseName = paste('pg', tolower(substr(dataCenter,1,1)), strsplit(databaseName,'pg')[[1]][2], sep='')
    # }
    else{
      databaseName = paste(tolower(substr(dataCenter,1,1)), databaseName, sep='')
    }
  }
  return(databaseName)
}

# clean query
cleanQueries = function(dataframe)
{
  queryCharacter=as.character(dataframe$query)
  queryCharacterClean=tolower(queryCharacter)
  queryCharacterClean=gsub("null |null|,", "",queryCharacterClean)
  queryCharacterClean=gsub("\t|\r|\v|\n", "",queryCharacterClean)
  queryCharacterClean=gsub("'[-a-zA-Z0-9 &/:_\\.]+'", "",queryCharacterClean)
  queryCharacterClean=gsub("[0-9\\.,]+", "",queryCharacterClean)
  queryCharacterClean=gsub("(IN )\\([-a-zA-Z0-9 ,/:_\\.]+\\)", "",queryCharacterClean)
  queryCharacterClean=gsub("(NULL)|(TRUE)|(FALSE)", "",queryCharacterClean)
  queryCharacterClean=gsub("\\s+", " ",queryCharacterClean)
  queryCharacterCleanFactor=gsub("where.*","",queryCharacterClean, fixed=FALSE)
  #  queryCharacterClean=gsub("  ", "@",queryCharacterClean)
  #  queryCharacterClean=gsub("@", "",queryCharacterClean)
  dataFrame=cbind(dataframe, queryCharacterCleanFactor)
  return(dataFrame)
}

# query parsing function to only keep the query type (e.g., delete, insert, update, select) and table name
parseQuery = function(query){
  Sys.setlocale('LC_ALL','C') 
  queryList = strsplit(query, " ")
  
  if(startsWith(query, "delete")){
    result = gsub("\\(", "", paste("delete from", queryList[[1]][3], sep=" "))
  } else if(startsWith(query, "insert")){
    result = gsub("\\(", "", paste("insert into", queryList[[1]][3], sep=" "))
  } else if(startsWith(query, "update")){
    result = gsub("\\(", "", paste("update", queryList[[1]][2], sep=" "))
  } else if(startsWith(query, "select")){
    result = gsub("\\(", "", paste("select xxx from", queryList[[1]][match('from',unlist(queryList))+1], sep=" "))
  } else{
    result = NULL
  }
  return(result)
}

# get query by query type and query table name
getQuery = function(queryType, queryTable){
  queryType = tolower(queryType)
  queryTable = tolower(queryTable)
  if(queryType=="delete"){
    result = paste("delete from", queryTable, sep=" ")
  } else if(queryType=="insert"){
    result = paste("insert into", queryTable, sep=" ")
  } else if(queryType=="update"){
    result = paste("update", queryTable, sep=" ")
  } else if(queryType=="select"){
    result = paste("select xxx from", queryTable, sep=" ")
  } else{
    result = NULL
  }
  return(result)
}

# earliest date
getEarliestDate = function(dts){
  if(length(dts)){
    return(toString(as.Date(min(dts))))
  }else{
    return('')
  }
}

# latest date
getLatestDate = function(dts){
  if(length(dts)){
    return(toString(as.Date(max(dts))))
  }else{
    return('')
  }
}

# parse account count
getAccountCount = function(accountcount){
  list = strsplit(gsub(" ", "", paste(accountcount, collapse=",")), ",")
  temp = as.data.frame(matrix(0, nrow=1, ncol=2))
  colnames(temp) = c("account", "count")
  for (i in list[[1]]){
    temp = rbind(temp, unlist(strsplit(gsub("\\)", "", i), "\\(")))
  }
  temp = temp[-1,]
  temp = ddply(temp, .(account), summarise, totalcount=sum(as.numeric(count)))
  temp = temp[order(temp$totalcount, decreasing=TRUE),]
  
  # concatenate into a single string
  account_count = ""
  for(j in 1:nrow(temp)){
    if(account_count!=""){
      account_count = paste(account_count, ', ', temp[j,1], '(', temp[j,2], ')', sep='')
    }else{
      account_count = paste(account_count, temp[j,1], '(', temp[j,2], ')', sep='')
    }
  }
  return(account_count)
}

# convert timeout data to timereportdaily
convertTimeout = function(data_timeout, extension){
  if(nrow(data_timeout)>0){
    # clean data
    data1 = cleanQueries(data_timeout)
    data1 = subset(data1, select = c(dts,duration,accountcode,usercode,brokerip,databaseip,databasename,queryCharacterCleanFactor))
    
    # summarize data
    data_timeout2 = as.data.frame(matrix(0, 1, nco=6))
    colnames(data_timeout2) = c('dts','query','timeoutcount','totalcount','timeoutpercentage','accountcount')
    
    data_q = split(data1, data1$queryCharacterCleanFactor, drop = TRUE)
    for(i in 1:length(data_q)){
      data2 = data_q[[i]]
      tmp = as.data.frame(table(data2$accountcode))
      colnames(tmp) = c("accountcode", "freq")
      tmp = tmp[order(tmp$freq, decreasing=TRUE),]
      account_count = ""
      for(j in 1:nrow(tmp)){
        if(account_count!=""){
          account_count = paste(account_count, ', ', tmp$accountcode[j], '(', tmp$freq[j], ')', sep='')
        }else{
          account_count = paste(account_count, tmp$accountcode[j], '(', tmp$freq[j], ')', sep='')
        }
      }
      timeoutcount = nrow(data2)
      totalcount = nrow(data1[which(data1$queryCharacterCleanFactor==data2$queryCharacterCleanFactor[1]),])
      timeoutpercentage = round(timeoutcount / totalcount, digits=4)
      data_timeout2 = rbind(data_timeout2, c(substr(data2$dts[1],1,10), toString(data2$queryCharacterCleanFactor[1]), timeoutcount, totalcount, timeoutpercentage, account_count))
    }
    
    data_timeout2 = data_timeout2[-1,]
    rownames(data_timeout2) = c()
    data_timeout2 = data_timeout2[order(as.numeric(data_timeout2$timeoutcount), decreasing=TRUE),]
    
    # parse query
    data_timeout2$query = sapply(data_timeout2$query, parseQuery, simplify = TRUE, USE.NAMES = FALSE)
    data_timeout2 = data_timeout2[which(data_timeout2$query!="NULL"),]
    data_timeout2$query = unlist(data_timeout2$query)
    
    data_reportdaily = ddply(data_timeout2, .(query), summarise, 
                             #TimeoutCount=sum(as.numeric(timeoutcount)), TotalCount=sum(as.numeric(totalcount)), EarliestDate=getEarliestDate(dts), LatestDate=getLatestDate(dts), AccountCount=getAccountCount(accountcount), 
                             TimeoutCount=sum(as.numeric(timeoutcount)), AccountCount=getAccountCount(accountcount))
  } else{
    data_reportdaily = as.data.frame(matrix(0, 1, nco=3))
  }
  colnames(data_reportdaily) = c("query", paste("TimeoutCount", extension), paste("AccountCount", extension))
  
  # data_reportdaily = data_reportdaily[!is.na(names(data_reportdaily))]
  #data_reportdaily = data_reportdaily[which(data_reportdaily$query!=0),]
  return(data_reportdaily[,1:3])
}


# convert timeout data to timereportdaily
convertTimeout2 = function(data_timeout, extension){
  # clean data
  data_timeout2 = cleanQueries(data_timeout)
  data_timeout2 = subset(data_timeout2, select = c(dts,duration,accountcode,usercode,brokerip,databaseip,databasename,queryCharacterCleanFactor))
  
  # parse query
  data_timeout2$query = sapply(as.character(data_timeout2$queryCharacterCleanFactor), parseQuery, simplify = TRUE, USE.NAMES = FALSE)
  data_timeout2 = data_timeout2[which(data_timeout2$query!="NULL"),]
  data_timeout2$query = unlist(data_timeout2$query)
  
  data_reportdaily1 = ddply(data_timeout2, .(query), summarise, accountcode='All accounts', TimeoutCount=length(duration))
  data_reportdaily2 = ddply(data_timeout2, .(query, accountcode), summarise, TimeoutCount=length(duration))
  data_reportdaily = rbind(data_reportdaily1,data_reportdaily2)

  colnames(data_reportdaily) = c("Query", "Account", paste("TimeoutCount", extension))
  return(data_reportdaily[,1:3])
}


# parse top query and recalculate rankings
parseTopQuery = function(dataframe, rankMode, truncated){
  # parse query
  if(truncated){
    dataframe$query = sapply(dataframe$query, parseQuery, simplify = TRUE, USE.NAMES = FALSE)
    dataframe = dataframe[which(dataframe$query!="NULL"),]
    dataframe$query = unlist(dataframe$query)
  }
  
  # recalculate rankings for each database and overall
  result = ddply(dataframe, .(dts, databasename, query), summarise, TotalCount=sum(totalcount), TotalDuration=sum(totalcount*meanduration), MeanDuration=sum(totalcount*meanduration)/sum(totalcount), minduration=min(minduration), 
                 firstquartileduration=mean(firstquartileduration), medianduration=mean(medianduration), thirdquartileduration=mean(thirdquartileduration), maxduration=max(maxduration), sdduration=mysd(sdduration))
  result[,6:12] = round(sapply(result[,6:12], as.numeric), digits=2)
  
  colnames(result)[4] = 'totalcount'
  colnames(result)[5] = 'totalduration'
  colnames(result)[6] = 'meanduration'
  if(rankMode=='CNT'){
    result = ddply(result, .(dts, databasename), transform, queryrank = rank(-totalcount, ties.method = "first"))
  }else if(rankMode=='MAS'){
    result = ddply(result, .(dts, databasename), transform, queryrank = rank(-totalduration, ties.method = "first"))
  }
  
  result = result[order(result$dts, result$databasename, result$queryrank),]
  
  # output
  return(result)
}