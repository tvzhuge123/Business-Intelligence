timeoutReport_AnalyzeData = function(dataCenter, dateRange, excludeWeekend){
  # input and preprocess
  # dataframe = read.csv(paste("/home/yan/RShiny/data/TxnlogReportDaily_", dataCenter, ".csv", sep=""), header = TRUE, check.names=FALSE, stringsAsFactors=FALSE)
  # query = paste("select * from txnlogreportdaily where",  
  #               " dts between ", "'", dateRange[1], "' and '", dateRange[2], "'",  
  #               sep="")
  # dataframe = loadLogData(query, dataCenter)
  # dataframe = dataframe[which(dataframe$timeoutcount!=0),]
  
  # dataCenter = 'SG'
  # dateRange = c('2018-05-29','2018-05-29')
  # excludeWeekend = FALSE
  
  if(as.numeric(as.Date(dateRange[2])-as.Date(dateRange[1]))>dataRangeLimit){
    return(error_var_date)
  }
  
  ################### timeout summary
  query = paste("select * from txnlogreportdaily where",  
                " dts between ", "'", dateRange[1], " 00:00:00' and '", dateRange[2], " 23:59:59' order by dts asc", 
                sep="")
  data_reportdaily1 = loadLogData(query, dataCenter)
  
  # verify connection to dc
  if(!is.data.frame(data_reportdaily1)){return(list(error_var_noaccess,error_var_noaccess))}
  
  # exclude weekends data
  if(excludeWeekend){data_reportdaily1 = excludeWeekendFunc(data_reportdaily1)}
  
  # check if there is data
  if(nrow(data_reportdaily1)==0){return(list(error_var_nodata,error_var_nodata))}
  
  #write.csv(data_reportdaily1, "/home/shiny/txnlogreportdaily.csv", row.names=FALSE)
  
  # parse query
  data_reportdaily1$query = sapply(data_reportdaily1$query, parseQuery, simplify = TRUE, USE.NAMES = FALSE)
  data_reportdaily1 = data_reportdaily1[which(data_reportdaily1$query!="NULL"),]
  data_reportdaily1$query = unlist(data_reportdaily1$query)
  
  # analysis
  data_reportdaily1 = ddply(data_reportdaily1, .(query), summarise, 
                            #TimeoutCount=sum(as.numeric(timeoutcount)), TotalCount=sum(as.numeric(totalcount)), EarliestDate=getEarliestDate(dts), LatestDate=getLatestDate(dts), AccountCount=getAccountCount(accountcount), 
                            TimeoutCount=sum(as.numeric(timeoutcount)), TotalCount=sum(as.numeric(totalcount)), AccountCount=getAccountCount(accountcount), 
                            MeanDuration=sum(totalcount*meanduration)/sum(totalcount), MinDuration=min(minduration), FirstquartileDuration=mean(firstquartileduration), MedianDuration=mean(medianduration), 
                            ThirdquartileDuration=mean(thirdquartileduration), MaxDuration=max(maxduration), StandardDeviation=mysd(sdduration))
  
  ################### timeout data
  query = paste("select * from txnlogtimeout where",  
                " duration>=60000", 
                " and dts between ", "'", dateRange[1], " 00:00:00' and '", dateRange[2], " 23:59:59' order by query asc, dts asc", 
                sep="")
  data_timeout = loadLogData(query, dataCenter)
  
  # verify connection to dc
  if(!is.data.frame(data_timeout)){return(list(error_var_noaccess,error_var_noaccess))}
  
  # exclude weekends data
  if(excludeWeekend){data_timeout = excludeWeekendFunc(data_timeout)}
  
  # check if there is data
  if(nrow(data_timeout)==0){return(list(error_var_nodata,error_var_nodata))}
  
  data_timeout$accountcode = replace(data_timeout$accountcode, is.na(data_timeout$accountcode), "N/A")
  data_timeout$usercode = replace(data_timeout$usercode, is.na(data_timeout$usercode), "N/A")
  data_timeout$query = replace(data_timeout$query, is.na(data_timeout$query), "N/A")
  data_timeout$exception = replace(data_timeout$exception, is.na(data_timeout$exception), "N/A")
  
  #write.csv(data_timeout, "/home/shiny/txnlogtimeout.csv", row.names=FALSE)
  
  ################### summarize timeout data
  data_timeout1 = data_timeout[which(data_timeout$duration<120000),]
  data_timeout2 = data_timeout[which(data_timeout$duration>=120000),]
  
  data_reportdaily2_1 = convertTimeout(data_timeout1, "60-120s")
  data_reportdaily2_2 = convertTimeout(data_timeout2, ">120s")
  
  tmp1 = nrow(data_reportdaily2_1[which(data_reportdaily2_1$query!=0),])
  tmp2 = nrow(data_reportdaily2_2[which(data_reportdaily2_2$query!=0),])
  
  ################### collect them together
  if(tmp1!=0 & tmp2==0){
    data_reportdaily2 = merge(x = data_reportdaily2_1, y = data_reportdaily2_2, by = "query", all.x = TRUE)
  } else if(tmp1==0 & tmp2!=0){
    data_reportdaily2 = merge(x = data_reportdaily2_1, y = data_reportdaily2_2, by = "query", all.y = TRUE)
  } else {
    data_reportdaily2 = merge(x = data_reportdaily2_1, y = data_reportdaily2_2, by = "query", all = TRUE)
  }
  
  data_reportdaily2$`TimeoutCount 60-120s` = replace(data_reportdaily2$`TimeoutCount 60-120s`, is.na(data_reportdaily2$`TimeoutCount 60-120s`), 0)
  data_reportdaily2$`AccountCount 60-120s` = replace(data_reportdaily2$`AccountCount 60-120s`, is.na(data_reportdaily2$`AccountCount 60-120s`), "")
  data_reportdaily2$`TimeoutCount >120s` = replace(data_reportdaily2$`TimeoutCount >120s`, is.na(data_reportdaily2$`TimeoutCount >120s`), 0)
  data_reportdaily2$`AccountCount >120s` = replace(data_reportdaily2$`AccountCount >120s`, is.na(data_reportdaily2$`AccountCount >120s`), "")
  data_reportdaily2[,c('TotalCount','MeanDuration', 'MinDuration', 'FirstquartileDuration', 'MedianDuration', 'ThirdquartileDuration', 'MaxDuration', 'StandardDeviation')] = c(0,0,0,0,0,0,0,0)
  
  if(nrow(data_reportdaily2[which(data_reportdaily2$query!=0),])!=0){
    for(q in unique(data_reportdaily2$query)){
      data_reportdaily2[which(data_reportdaily2$query==q),]$TotalCount = data_reportdaily1[which(data_reportdaily1$query==q),]$TotalCount
      data_reportdaily2[which(data_reportdaily2$query==q),]$MeanDuration = data_reportdaily1[which(data_reportdaily1$query==q),]$MeanDuration
      data_reportdaily2[which(data_reportdaily2$query==q),]$MinDuration = data_reportdaily1[which(data_reportdaily1$query==q),]$MinDuration
      data_reportdaily2[which(data_reportdaily2$query==q),]$FirstquartileDuration = data_reportdaily1[which(data_reportdaily1$query==q),]$FirstquartileDuration
      data_reportdaily2[which(data_reportdaily2$query==q),]$MedianDuration = data_reportdaily1[which(data_reportdaily1$query==q),]$MedianDuration
      data_reportdaily2[which(data_reportdaily2$query==q),]$ThirdquartileDuration = data_reportdaily1[which(data_reportdaily1$query==q),]$ThirdquartileDuration
      data_reportdaily2[which(data_reportdaily2$query==q),]$MaxDuration = data_reportdaily1[which(data_reportdaily1$query==q),]$MaxDuration
      data_reportdaily2[which(data_reportdaily2$query==q),]$StandardDeviation = data_reportdaily1[which(data_reportdaily1$query==q),]$StandardDeviation
    }
  }
  colnames(data_reportdaily2)[1] = 'Query'
  data_reportdaily2[,7:13] = round(sapply(data_reportdaily2[,7:13], as.numeric), digits=2)
  data_reportdaily2 = data_reportdaily2[which(data_reportdaily2$Query!=0),]
  
  # output
  data_timeout$dts = as.character(data_timeout$dts)
  return(list(data_reportdaily2, data_timeout))
}