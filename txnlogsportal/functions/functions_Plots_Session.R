plot_session = function(dateRange, excludeWeekend, sessionName, dataCenter){
  if(as.numeric(as.Date(dateRange[2])-as.Date(dateRange[1]))>dataRangeLimit_plot){
    return(error_var_date_plot)
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
  
  # get hour and houst ip
  Dataframe$`Host IP` = sapply(Dataframe$HostName, function(x) paste(strsplit(toString(x), ":")[[1]][2], strsplit(toString(x), ":")[[1]][3], sep=":"))
  
  # main process starts from here
  dataframe = Dataframe[, c("Host IP", "Hour", sessionName)]
  colnames(dataframe)[3] = "sessions"
  
  # summary data
  summary = ddply(dataframe, .(`Host IP`, Hour), summarise, sessions=round(mean(sessions), digit=2))
  summary_plot = summary[which(summary$sessions>0),]
  
  # line plot
  title = paste(sessionName, " - ", dataCenter, " Hourly Average", " - ", dateRange[1], " to ", dateRange[2], sep="")
  if (dim(summary_plot)[1] <= 0){
    title = paste(title, " - NO DATA FOUND", sep="")
    return(error_var_other)
  }
  
  myPlot = ggplot(summary_plot, aes(x=Hour, y=sessions, group=`Host IP`, colour=`Host IP`)) + 
    geom_line(size=0.7) + 
    scale_x_continuous(breaks = 0:23) + 
    scale_y_continuous(breaks = pretty(summary_plot$sessions, n = 5), labels = comma) + 
    labs(y="Session Count") + 
    ggtitle(title) + 
    theme(plot.title = element_text(size=9, hjust=0.5))
  return(myPlot)
}