plot_topquery_3d = function(dcflag, mytime_range, excludeWeekend, obj, mydurationfilter, myrankmode, searchMode, queryRank, queryType, queryTable, truncated){
  if(as.numeric(as.Date(mytime_range[2])-as.Date(mytime_range[1]))>dataRangeLimit_plot){
    return(error_var_date_plot)
  } else if(as.numeric(as.Date(mytime_range[2])-as.Date(mytime_range[1]))<=0){
    return(error_var_other)
  }
  
  # input and preprocess
  mytime = mytime_range[2]
  query = paste("select * from txnlogtopquery where", 
                " durationfilter=", mydurationfilter, 
                " and rankmode=", "'", myrankmode, "'", 
                " and dts between ", "'", mytime_range[1], "' and '", mytime_range[2], "'", 
                sep="")
  dataframe = loadLogData(query, dcflag)
  
  # verify connection to dc
  if(!is.data.frame(dataframe)){return(error_var_noaccess)}
  
  # exclude weekends data
  if(excludeWeekend){dataframe = excludeWeekendFunc(dataframe)}
  
  # check if there is data
  if(nrow(dataframe)==0){return(error_var_nodata)}
  
  dataframe = parseTopQuery(dataframe, myrankmode, truncated)
  
  #dataframe_q = split(dataframe,dataframe$query, drop = TRUE)
  if(searchMode=='Query rank'){
    myrank = queryRank
    myquery = dataframe[which(dataframe$databasename=="all" & dataframe$dts==mytime & dataframe$queryrank==myrank),]$query
  } else {
    myquery = getQuery(queryType, queryTable)
    myrank = dataframe[which(dataframe$databasename=="all" & dataframe$dts==mytime & dataframe$query==myquery),]$queryrank
  }
  
  dataframe2 = dataframe[which(dataframe$query==myquery),]
  
  if(dcflag=='L3'){
    if(length(which(!(grepl("sintercat",dataframe2$databasename) | grepl("all",dataframe2$databasename))))>0)
    {dataframe2 = dataframe2[which(!grepl("sintercat",dataframe2$databasename)),]}
  }else if(dcflag=='SG'){
    if(length(which(!(grepl("lintercat",dataframe2$databasename) | grepl("all",dataframe2$databasename))))>0)
    {dataframe2 = dataframe2[which(!grepl("lintercat",dataframe2$databasename)),]}
  }
  
  # check if there is data
  if(nrow(dataframe2)==0){return(error_var_nodata)}
  
  tmp = as.data.frame(unique(dataframe2$dts))
  tmp = as.data.frame(tmp[order(tmp),])
  plot_time = cbind(tmp, 1:length(unique(dataframe2$dts)))
  colnames(plot_time) = c('dts','plot_time')
  
  tmp = as.data.frame(unique(dataframe2$databasename))
  tmp = as.data.frame(tmp[order(tmp),])
  plot_dbname = cbind(tmp, 1:length(unique(dataframe2$databasename)))
  colnames(plot_dbname) = c('databasename','plot_dbname')
  
  dataframe2 = merge(x = dataframe2, y = plot_time, by = "dts", all.x = TRUE, sort=FALSE)
  dataframe2 = merge(x = dataframe2, y = plot_dbname, by = "databasename", all.x = TRUE, sort=FALSE)
  
  dataframe_plot = subset(dataframe2, select = c(plot_time,plot_dbname,totalcount,meanduration,minduration,firstquartileduration,medianduration,thirdquartileduration,maxduration,sdduration))
  dataframe_plot2 = acast(dataframe_plot, plot_time~plot_dbname, value.var=obj)
  dataframe_plot3 = acast(dataframe_plot, plot_time~plot_dbname, value.var="totalcount")
  
  #png(paste(folder,dcflag," ",mydurationfilter," ",myrankmode," ",mytime," top",myrank," 3D ",obj,".png",sep = ""),height=800,width=1400,pointsize=18)
  #par(mar=c(5,6,4,4))
  image2D(dataframe_plot2, clab = paste(obj,"/ms","\n", "(frequency)",sep = ""),axes = FALSE, xlab='',ylab='', cex.main=0.9, 
          main=paste(dcflag," ",mytime," Top 50 queries #",myrank," (longer than ",mydurationfilter," ms and ranked by ",myrankmode,")", "\n", substr(dataframe2$query[1],1,100), sep = ""))
  x = seq(0,1,1/(length(plot_time$plot_time)-1))
  y = seq(0,1,1/(length(plot_dbname$databasename)-1))
  mtext(text=format(as.Date(plot_time$dts),format="%b-%d %a"), side=1, line=0.3, las=2, cex=0.9, at=x) # x axis
  mtext(text=plot_dbname$databasename, side=2, line=0.3, las=1, cex=0.9, at=y) # y axis
  text(arrange(expand.grid(x=x,y=y),y), labels=dataframe_plot3, cex= 0.8) # frequency
}