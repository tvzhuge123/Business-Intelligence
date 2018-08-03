sessionReport_AnalyzeData = function(dateRange, excludeWeekend, sessionName, dataCenter){
  if(as.numeric(as.Date(dateRange[2])-as.Date(dateRange[1]))>dataRangeLimit){
    return(error_var_date)
  }
  
  if(dataCenter=="L3"){
    dataCenterIP = "192.168.120"
  }else if(dataCenter=="SG"){
    dataCenterIP = "192.168.140"
  }
  
  # pull data
  query = paste("select dts, DATEPART(HOUR, dts) Hour, HostName, ", sessionName, " from MonitorInfo where",  
                " HostName like '%", dataCenterIP, "%'", 
                " and ", sessionName, ">=0", 
                " and dts between ", "'", dateRange[1], " 00:00:00' and '", dateRange[2], " 23:59:59' order by dts asc", 
                sep="")
  Dataframe = loadDataSQLServer(query)
  
  # verify connection to dc
  if(!is.data.frame(Dataframe)){return(error_var_noaccess)}
  
  # exclude weekends data
  if(excludeWeekend){Dataframe = excludeWeekendFunc(Dataframe)}
  
  # check if there is data
  if(nrow(Dataframe)==0){return(error_var_nodata)}
  
  # get host ip
  # Dataframe$Hour = format(strptime(Dataframe$dts,"%Y-%m-%d %H:%M:%S"),'%H')
  Dataframe$`Host IP` = sapply(Dataframe$HostName, function(x) paste(strsplit(toString(x), ":")[[1]][2], strsplit(toString(x), ":")[[1]][3], sep=":"))
  
  # main process starts from here
  dataframe = Dataframe[, c("Host IP", "Hour", sessionName)]
  colnames(dataframe)[3] = "sessions"
  
  # summary data
  summary = ddply(dataframe, .(`Host IP`, Hour), summarise, sessions=round(mean(sessions), digit=2))
  
  # unstack
  result = reshape(summary, idvar = "Host IP", timevar = "Hour", direction = "wide")
  
  # Add missing column
  tmp1 = c("Host IP")
  for (i in 0:23)
  {
    tmp1 = c(tmp1, paste("sessions.", i, sep=""))
  }
  missing = setdiff(tmp1, names(result))
  result[missing] = 0
  result = result[tmp1]
  
  # Change column name
  tmp2 = c("Host IP")
  for (i in 0:23)
  {
    tmp2 = c(tmp2, paste(i, "To", i+1, sep=""))
  }
  colnames(result) = tmp2
  
  # delete all NAs and 0s
  result[is.na(result)] = 0
  result = result[apply(result[,-1], 1, function(x) !all(x==0)),]
  if (dim(result)[1] <= 0){
    result[1,] = 0
  }
  
  # output
  return(result)
}