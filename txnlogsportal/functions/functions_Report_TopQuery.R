topQuery_AnalyzeData = function(dataCenter, databaseName, dateRange, excludeWeekend, truncated, durationFilter, rankMode, rankRange){
  if(as.numeric(as.Date(dateRange[2])-as.Date(dateRange[1]))>dataRangeLimit){
    return(rep(error_var_date,13))
  }
  
  # input and preprocess
  databaseName = databaseNameParse(dataCenter, databaseName);
  
  #dataframe = read.csv(paste("/home/yan/RShiny/data/topquery_", dataCenter, ".csv", sep=""), header = TRUE, check.names=FALSE, stringsAsFactors=FALSE)
  #dataframe = dataframe[which(dataframe$durationfilter==durationFilter & dataframe$rankmode==rankMode & dataframe$databasename==databaseName),]
  query = paste("select * from txnlogtopquery where", 
                " databasename=", "'", databaseName, "'", 
                " and durationfilter=", durationFilter, 
                " and rankmode=", "'", rankMode, "'", 
                " and dts between ", "'", dateRange[1], "' and '", dateRange[2], "'", 
                sep="")
  dataframe = loadLogData(query, dataCenter)
  
  # verify connection to dc
  if(!is.data.frame(dataframe)){return(error_var_noaccess)}
  
  # exclude weekends data
  if(excludeWeekend){dataframe = excludeWeekendFunc(dataframe)}
  
  # check if there is data
  if(nrow(dataframe)==0){return(error_var_nodata)}
  
  # parse query
  if(truncated){
    dataframe$query = sapply(dataframe$query, parseQuery, simplify = TRUE, USE.NAMES = FALSE)
    dataframe = dataframe[which(dataframe$query!="NULL"),]
    dataframe$query = unlist(dataframe$query)
  }
  
  # analysis
  result = ddply(dataframe, .(query), summarise, TotalDuration=sum(totalcount*meanduration), TotalCount=sum(totalcount), MeanDuration=sum(totalcount*meanduration)/sum(totalcount), 
                 MinDuration=min(minduration), FirstquartileDuration=mean(firstquartileduration), MedianDuration=mean(medianduration), 
                 ThirdquartileDuration=mean(thirdquartileduration), MaxDuration=max(maxduration), StandardDeviation=mysd(sdduration))
  result[,2] = round(sapply(result[,2], as.numeric), digits=0)
  result[,4:10] = round(sapply(result[,4:10], as.numeric), digits=2)
  
  if(rankMode=='MAS'){
    result = result[order(result$TotalDuration, decreasing = TRUE),]
  }else if(rankMode=='CNT'){
    result = result[order(result$TotalCount, decreasing = TRUE),]
  }
  
  # add rank column
  #result = cbind(1:nrow(result), result[,-2])
  #colnames(result)[1] = 'Rank'
  result = result[rankRange[1]:min(rankRange[2], nrow(result)),]
  rownames(result) = NULL
  colnames(result)[1] = 'Query'
  
  # output
  return(result)
}