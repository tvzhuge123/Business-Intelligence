accountActivity_AnalyzeData = function(dataCenter, databaseName, dateRange, excludeWeekend){
  if(as.numeric(as.Date(dateRange[2])-as.Date(dateRange[1]))>dataRangeLimit){
    return(list(rep(error_var_date,11)))
  }
  
  # input and preprocess
  databaseName = databaseNameParse(dataCenter, databaseName);
  mytime_range = as.character(seq(as.Date(dateRange[1]), as.Date(dateRange[2]), by = "+1 day"))
  
  # dataframe = read.csv(paste("/home/yan/RShiny/data/txnlogsummarydbaccount_", dataCenter, ".csv", sep=""), header = TRUE, check.names=FALSE, stringsAsFactors=FALSE)
  # dataframe = dataframe[which(dataframe$databasename==databaseName & grepl(paste(mytime_range, collapse = "|"), dataframe$dts)),]
  query = paste("select * from txnlogsummarydbaccount where",  
                " databasename=", "'", databaseName, "'",
                " and dts between ", "'", dateRange[1], "' and '", dateRange[2], "'", 
                sep="")
  dataframe = loadLogData(query, dataCenter)
  
  # verify connection to dc
  if(!is.data.frame(dataframe)){return(error_var_noaccess)}
  
  # exclude weekends data
  if(excludeWeekend){dataframe = excludeWeekendFunc(dataframe)}
  
  # check if there is data
  if(nrow(dataframe)==0){return(error_var_nodata)}
  
  # analysis
  result = ddply(dataframe, .(account), summarise, UserCount=sum(accounttotaluser), TotalDuration=sum(as.numeric(accounttotaltxntime)), 'TotalDuration%'=sum(as.numeric(accounttotaltxntime))/sum(as.numeric(databasetotaltxntime)), 
                 TxnCount=sum(as.numeric(accounttotaltxn)), 'TxnCount%'=sum(as.numeric(accounttotaltxn))/sum(as.numeric(databasetotaltxn)), MeanDuration=sum(as.numeric(accounttotaltxntime))/sum(as.numeric(accounttotaltxn)), 
                 MinDuration=min(minduration), FirstquartileDuration=mean(firstquartileduration), MedianDuration=mean(medianduration), 
                 ThirdquartileDuration=mean(thirdquartileduration), MaxDuration=max(maxduration), StandardDeviation=mysd(sdduration))
  result[,4] = round(sapply(result[,4]*100, as.numeric), digits=3)
  result[,6] = round(sapply(result[,6]*100, as.numeric), digits=3)
  result[,7:13] = round(sapply(result[,7:13], as.numeric), digits=2)
  
  result = result[order(result$TotalDuration, decreasing = TRUE),]
  
  # add rank column
  #result = cbind(1:nrow(result), result[,-2])
  #colnames(result)[1] = 'Rank'
  rownames(result) = NULL
  colnames(result)[1] = 'Account'
  
  # output
  return(result)
}