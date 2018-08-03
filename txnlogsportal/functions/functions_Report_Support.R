productReport_AnalyzeData = function(dateRange, excludeWeekend, myAccountCode){
  if(as.numeric(as.Date(dateRange[2])-as.Date(dateRange[1]))>dataRangeLimit){return(list(error_var_date,error_var_date,error_var_date))}
  myAccountCode = gsub(" ", "", myAccountCode)
  
  ################### timeout data
  if(myAccountCode==""){
    query = paste("select * from txnlogtimeout where",  
                  " duration>=60000", 
                  " and dts between ", "'", dateRange[1], " 00:00:00' and '", dateRange[2], " 23:59:59' order by query asc, dts asc", 
                  sep="")
  } else {
    query = paste("select * from txnlogtimeout where",  
                  " duration>=60000", 
                  " and lower(accountcode)='", myAccountCode, "'",
                  " and dts between ", "'", dateRange[1], " 00:00:00' and '", dateRange[2], " 23:59:59' order by query asc, dts asc", 
                  sep="")
  }

  data_timeout_L3 = loadLogData(query, 'L3')
  data_timeout_SG = loadLogData(query, 'SG')
  
  # verify connection to dc
  if(!is.data.frame(data_timeout_L3) || !is.data.frame(data_timeout_SG)){return(list(error_var_noaccess,error_var_noaccess,error_var_noaccess))}
  
  data_timeout = rbind(data_timeout_L3, data_timeout_SG)
  
  # exclude weekends data
  if(excludeWeekend){data_timeout = excludeWeekendFunc(data_timeout)}
  
  # check if there is data
  if(nrow(data_timeout)==0){return(list(error_var_nodata,error_var_nodata,error_var_nodata))}
  
  data_timeout$accountcode = replace(data_timeout$accountcode, is.na(data_timeout$accountcode), "N/A")
  data_timeout$usercode = replace(data_timeout$usercode, is.na(data_timeout$usercode), "N/A")
  data_timeout$query = replace(data_timeout$query, is.na(data_timeout$query), "NA")
  
  #write.csv(data_timeout, "/home/shiny/txnlogtimeout.csv", row.names=FALSE)
  
  ################### summarize timeout data
  data_timeout1 = data_timeout[which(data_timeout$duration<120000),]
  data_timeout2 = data_timeout[which(data_timeout$duration>=120000),]
  #data_timeout2 = data_timeout
  
  data_reportdaily2_1 = convertTimeout2(data_timeout1, "60-120s")
  data_reportdaily2_2 = convertTimeout2(data_timeout2, ">120s")
  
  ################### merge them together
  data_reportdaily2 = merge(x = data_reportdaily2_2, y = data_reportdaily2_1, by = c("Query","Account"), all = TRUE)
  data_reportdaily2$`TimeoutCount 60-120s` = replace(data_reportdaily2$`TimeoutCount 60-120s`, is.na(data_reportdaily2$`TimeoutCount 60-120s`), 0)
  data_reportdaily2$`TimeoutCount >120s` = replace(data_reportdaily2$`TimeoutCount >120s`, is.na(data_reportdaily2$`TimeoutCount >120s`), 0)
  data_reportdaily2 = data_reportdaily2[order(data_reportdaily2$Query, data_reportdaily2$Account, decreasing=FALSE),]

  result1 = data_reportdaily2[which(data_reportdaily2$Account=='All accounts'),][,-2]
  result1 = result1[order(result1$`TimeoutCount >120s`, result1$`TimeoutCount 60-120s`, decreasing=TRUE),]
  result2 = data_reportdaily2[which(data_reportdaily2$Account!='All accounts'),]
  
  # output
  data_timeout$dts = as.character(data_timeout$dts)
  return(list(result1, result2, data_timeout))
}