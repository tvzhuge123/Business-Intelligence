plot_accountActivity = function(dateRange, excludeWeekend, searchMode, accountRank_dataCenter, accountRank_database, rankMode, accountRank_rank, accountCode_database, accountCode_code){
  # dateRange=c('2018-04-25','2018-05-01')
  # excludeWeekend=FALSE
  # searchMode='Account Code'
  # accountRank_dataCenter = 'L3'
  # accountRank_database = 'InterCAT1'
  # rankMode = 'MAS'
  # accountCode_database = 'Overall'
  # accountCode_code = 'urgentc'
  #  
  # dataframe = read.csv(paste("/home/yan/RShiny/data/txnlogsummarydbaccount_L3.csv", sep=""), header = TRUE, check.names=FALSE, stringsAsFactors=FALSE)
  
  if(as.numeric(as.Date(dateRange[2])-as.Date(dateRange[1]))>30){return(list(error_var_date_plot))}
  
  if(searchMode=='Account Code' && gsub(" ", "", accountCode_code)==''){return(list(error_var_nodata))}
  
  # load data
  if(searchMode=='Account Rank'){
    myDataCenter = accountRank_dataCenter
    myDatabaseName = databaseNameParse(myDataCenter, accountRank_database);
    query = paste("select * from txnlogsummarydbaccount where",  
                  " databasename=", "'", myDatabaseName, "'",
                  " and dts between ", "'", dateRange[1], "' and '", dateRange[2], "'", 
                  sep="")
    dataframe = loadLogData(query, myDataCenter)
    
    # verify connection to dc
    if(!is.data.frame(dataframe)){return(list(error_var_noaccess))}
  } else {
    query = paste("select * from txnlogsummarydbaccount where",  
                  " dts between ", "'", dateRange[1], "' and '", dateRange[2], "'", 
                  " and lower(account)= lower(", "'", accountCode_code,  "')",
                  sep="")
    
    # find data center
    myDataCenter = 'L3'
    dataframe = loadLogData(query, myDataCenter)
    
    # verify connection to dc
    if(!is.data.frame(dataframe)){return(list(error_var_noaccess))}
    
    if(dim(dataframe)[1]==0){
      myDataCenter = 'SG'
      dataframe = loadLogData(query, myDataCenter)
    }
    
    #find database name
    if(accountCode_database=='InterCAT'){
      myDatabaseName = dataframe[which(grepl('intercat', dataframe$databasename) & dataframe$account==accountCode_code),]$databasename[1]
    } else {
      myDatabaseName = 'all'
    }
    dataframe = dataframe[which(dataframe$databasename==myDatabaseName),]
  }
  
  # exclude weekends data
  if(excludeWeekend){dataframe = excludeWeekendFunc(dataframe)}
  
  # check if there is data
  if(dim(dataframe)[1]==0){return(list(error_var_nodata))}
  
  # analysis
  result = ddply(dataframe, .(dts, account), summarise, UserCount=sum(accounttotaluser), TotalDuration=sum(as.numeric(accounttotaltxntime)), 'TotalDuration%'=sum(as.numeric(accounttotaltxntime))/sum(as.numeric(databasetotaltxntime)), 
                 TxnCount=sum(as.numeric(accounttotaltxn)), 'TxnCount%'=sum(as.numeric(accounttotaltxn))/sum(as.numeric(databasetotaltxn)), MeanDuration=sum(as.numeric(accounttotaltxntime))/sum(as.numeric(accounttotaltxn)), 
                 MinDuration=min(minduration), FirstquartileDuration=mean(firstquartileduration), MedianDuration=mean(medianduration), 
                 ThirdquartileDuration=mean(thirdquartileduration), MaxDuration=max(maxduration), StandardDeviation=mysd(sdduration))
  result[,5] = sapply(result[,5]*100, as.numeric)
  result[,7] = sapply(result[,7]*100, as.numeric)
  result[,8:14] = round(sapply(result[,8:14], as.numeric), digits=2)
  colnames(result)[1] = 'DTS'
  colnames(result)[2] = 'Account'
  
  # add rank for each account on each dts
  result1 = NULL
  for (dts in unique(result$DTS)){
    data = result[which(result$DTS==dts),]
    if(rankMode=='MAS'){
      data = data[order(data$TotalDuration, decreasing = TRUE),]
    } else {
      data = data[order(data$TxnCount, decreasing = TRUE),]
    }
    data$DTSAccountRank = 0:(dim(data)[1]-1)
    
    result1 = rbind(result1,data)
  }
  result1[,1] = as.character(result1[,1])
  rownames(result1) = NULL
  
  # filter to only keep given account
  if(searchMode=='Account Rank'){
    myAccountCode = result1[which(result1$DTS==dateRange[2] & result1$DTSAccountRank==accountRank_rank),]$Account[1]
  } else {
    myAccountCode = accountCode_code
  }
  result = result1[which(result1$Account==myAccountCode),c(1,5,7)]
  
  # add dummy value for unmatched rows
  date_seq = unique(result1$DTS)
  for (i in date_seq[!(date_seq %in% result$DTS)]){
    result = rbind(result,list(i,0,0))
  }
  result = result[order(result$DTS, decreasing=FALSE),]
  result$DTS = format(seq(min(as.Date(result$DTS)), max(as.Date(result$DTS)), length.out=nrow(result)), format="%b-%d %a")
  
  # ggplot
  myPlot = ggplot(result, aes(DTS, group = 1)) + 
    geom_line(aes(y = `TotalDuration%`, colour = "TotalDuration%")) + 
    geom_line(aes(y = `TxnCount%`, colour = "TxnCount%")) + 
    xlab("") + ylab("Percentage(%)") + 
    ggtitle(paste("Account activity of", myAccountCode, "on", myDataCenter, myDatabaseName, sep = " ")) +
    #scale_y_continuous(breaks = pretty(result$`TotalDuration%`, n = 6), labels = comma) +
    scale_x_discrete(breaks = format(seq(min(as.Date(result1$DTS, origin="1970-01-01")), max(as.Date(result1$DTS)), length.out=10), format="%b-%d %a")) +
    # theme_set(theme_gray(base_size = 6)) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(hjust = 0.5))
  
  return(myPlot)
}