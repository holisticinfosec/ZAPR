library(RJSONIO)
library(gdata) 
library(plyr)
library(rCharts)

setwd("C:/coding/R/ZapAPI")

zapapi <- fromJSON("http://localhost:8067/JSON/core/view/alerts/?zapapiformat=JSON&apikey=2v3tebdgojtcq3503kuoq2lq5g&formMethod=GET&baseurl=&start=&count=")
zapapi <- zapapi[['alerts']]

grabInfo<-function(var){
  print(paste("Alerts", var, sep=" "))  
  sapply(zapapi, function(x) returnData(x, var)) 
}

returnData<-function(x, var){
  if(!is.null( x[[var]])){
    return( trim(x[[var]]))
  }else{
    return(NA)
  }
}

ZapDataDF<-data.frame(sapply(1:19, grabInfo), stringsAsFactors=FALSE)

PluginID <- ZapDataDF[5]
CWEID <- ZapDataDF[6]
WASCID <- ZapDataDF[8]
Alert <- ZapDataDF[14]
Risk <- ZapDataDF[18]

reportDF <- cbind(Alert,Risk,WASCID,PluginID,CWEID)
colnames(reportDF) <- c("Alert","Risk","WASCID","PluginID","CWEID")

ct1 <- count(reportDF$Risk)
n1 <- hPlot(freq ~ x, group = 'freq', data = ct1, type = 'bar')

ct2 <- count(reportDF$CWEID)
n2 <- nPlot(freq ~ x, group = "freq", data = ct2, type = "pieChart")
