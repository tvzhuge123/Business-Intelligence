plot_topquery_2d = function(dcflag, mydurationfilter, myrankmode, searchMode, queryRank, queryType, queryTable, plotMode, plotMode_date, plotMode_dateRange, excludeWeekend, plotMode_database, truncated){
  # mydurationfilter=0
  # dcflag='L3'
  # myrankmode='CNT'
  # searchMode='Query rank'
  # queryRank=1
  # queryType='Select'
  # queryTable='patient'
  # plotMode='Dates'
  # plotMode_date=c('2016-10-01')
  # plotMode_dateRange=c('2017-09-27','2017-09-28')
  # plotMode_database='Overall'
  
  # input and preprocess
  if(plotMode=='Dates'){
    mydb = databaseNameParse(dcflag, plotMode_database);
    mytime = plotMode_dateRange[2]
    mytime_range = plotMode_dateRange
  } else {
    mytime = plotMode_date
    mytime_range = c(plotMode_date, plotMode_date)
  }
  
  if(as.numeric(as.Date(mytime_range[2])-as.Date(mytime_range[1]))>dataRangeLimit_plot){return(list(error_var_date_plot))}
  
  query = paste("select * from txnlogtopquery where", 
                " durationfilter=", mydurationfilter, 
                " and rankmode=", "'", myrankmode, "'", 
                " and dts between ", "'", mytime_range[1], "' and '", mytime_range[2], "'",
                sep="")
  dataframe = loadLogData(query, dcflag)
  
  # verify connection to dc
  if(!is.data.frame(dataframe)){return(list(error_var_noaccess))}
  
  # exclude weekends data
  if(plotMode=='Dates' & excludeWeekend){dataframe = excludeWeekendFunc(dataframe)}
  
  dataframe = parseTopQuery(dataframe, myrankmode, truncated)
  
  if(searchMode=='Query rank'){
    myrank = queryRank
    myquery = dataframe[which(dataframe$databasename=="all" & dataframe$dts==mytime & dataframe$queryrank==myrank),]$query
  } else {
    myquery = getQuery(queryType, queryTable)
    myrank = dataframe[which(dataframe$databasename=="all" & dataframe$dts==mytime & dataframe$query==myquery),]$queryrank
  }
  
  dataframe2 = dataframe[which(dataframe$query==myquery),]
  
  if(plotMode=='Dates'){
    dataframe2 = dataframe2[which(dataframe2$databasename==mydb),]
    dataframe2$dts = format(as.Date(dataframe2$dts),format="%b-%d %a")
    dataframe2$dts = factor(dataframe2$dts, levels = dataframe2$dts)
  } else {
    if(grepl('L3',dcflag)){
      if(length(which(!(grepl("sintercat",dataframe2$databasename) | grepl("all",dataframe2$databasename))))>0)
      {dataframe2 = dataframe2[which(!grepl("sintercat",dataframe2$databasename)),]}
    }else if(grepl('SG',dcflag)){
      if(length(which(!(grepl("lintercat",dataframe2$databasename) | grepl("all",dataframe2$databasename))))>0)
      {dataframe2 = dataframe2[which(!grepl("lintercat",dataframe2$databasename)),]}
    }
    dataframe2 = dataframe2[which(dataframe2$dts==mytime),]
    dataframe2$dts = format(as.Date(dataframe2$dts),format="%b-%d %a")
  }
  
  # check if there is data
  if(nrow(dataframe2)==0){return(list(error_var_nodata))}
  
  # ggplot
  if(plotMode=='Dates'){
    titlename = mydb
    if(titlename=='all'){
      titlename = paste(titlename, 'databases', sep=' ')
    }
    result = ggplot(dataframe2, aes(dts))
  } else {
    titlename = mytime
    result = ggplot(dataframe2, aes(databasename))
  }
  
  result = result +
    geom_boxplot(aes(ymin=firstquartileduration, lower=firstquartileduration, middle=medianduration, upper=thirdquartileduration, ymax=thirdquartileduration), stat="identity", lwd=0.2) +
    #scale_y_continuous(breaks = pretty(dataframe2$meanduration, n = 5), labels = comma, expand = c(0.1, 0)) +
    scale_y_continuous(breaks = pretty(seq(min(min(dataframe2$meanduration),min(dataframe2$firstquartileduration)),
                                           max(max(dataframe2$meanduration),max(dataframe2$thirdquartileduration)),
                                           length.out=10), n = 5), labels = comma, expand = c(0.1, 0)) +
    ggtitle(paste("Boxplot ",dcflag," "," Top 50 queries #",myrank," (longer than ",mydurationfilter," ms and ranked by ",myrankmode,") on ", titlename,"\n", substr(dataframe2$query[1],1,100), sep = "")) +
    labs(x="",y="Duration(ms)", size=0.1) +
    geom_point(aes(y=meanduration, colour = paste(" Mean Duration", "\n", "(Standard Deviation)", "\n", "(Frequency)", collapse="")), size=0.5, show.legend = TRUE) +
    scale_colour_manual("", values = c("red")) + 
    guides(colour = guide_legend(override.aes = list(size=1))) + 
    geom_text(aes(y = meanduration,label = round(meanduration, digits=0)),vjust = -3.3, size=1.6) + 
    #geom_text(aes(y = meanduration,label = format(sdduration, digits=1, scientific = TRUE)),vjust = -2, size=1.3) + 
    geom_text(aes(y = meanduration,label = paste("(",round(sdduration, digits=0),")",sep="")), vjust = -2, size=1.6) + 
    geom_text(aes(y = meanduration,label = paste("(",totalcount,")",sep="")),vjust = -0.7, size=1.6) + 
    theme_set(theme_gray(base_size = 6)) + 
    theme(axis.text.x = element_text(size=6, angle = 45, hjust = 1), plot.title = element_text(size=6, hjust=0.5), legend.title=element_text(size=6) , legend.text=element_text(size=6), 
          axis.text=element_text(size=6), axis.title=element_text(size=6))
  #ggsave(paste(folder,dcflag," ",mydurationfilter," ",myrankmode," ",mytime," top",myrank," BOX ",mydb,".png",sep = ""), plot = last_plot(), scale = 1, height=80, width=100*1.5, units = "mm")
  return(result)
}