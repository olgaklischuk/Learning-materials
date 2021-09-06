# install.packages("quantmod")
dir.create("~/OneDrive/Computer/H/Investing/data")
if(Sys.info()[4]=="MacBook-Air-Ulia.local"){comp="mac"}else{comp="asus"}

if(comp=="asus"){
  OneDrive<-"C:/Users/1/OneDrive/Computer/"}else{OneDrive<-"~/OneDrive/Computer/"}

if(comp=="mac"){setwd("~/OneDrive/Computer/H/Investing/data");wd<-"~/OneDrive/Computer/H/Investing/data"}else{setwd("~/OneDrive/Computer/H/Investing/data");wd<-"~/OneDrive/Computer/H/Investing/data"}
wd<-c("~/OneDrive/Computer/H/Investing/data/")

load(paste(wd,"/technology.RData",sep=""))
  library("quantmod");charts<-c("bar.chart","candle.chart","series")
  tickers<-c("^NYA","2357.TW","BABA","AMZN", "EPAM", #"LXFT",
             "TSLA","MSFT","AAPL","GOOG", "GOOGL","FB","INTC","CSCO","TSM","ORCL","SAP","ADBE","IBM","CRM")
  tickers.SHORTS<-c("BA","TGT","DIS","WMT","CVX","T")
  labels.tech<-c("^NYA","2357.TW","BABA","AMZN", "EPAM", #"LXFT",
                 "TSLA","MSFT","AAPL","GOOG", "GOOGL","FB","INTC","CSCO","TSM","ORCL","SAP","ADBE","IBM","CRM")
  labels.big.shorts<-c("BA","TGT","DIS","WMT","CVX","T")
 ##chartSeries######
    chartSeries.full<- function(x) {
       data(sample_matrix, package="xts")
       data <- as.xts(sample_matrix)
       cat("A simple xts object:\n")
       print(str(data))

         cat("chartSeries(data)\n")
       chartSeries(data)
       readline("Press <Enter> to continue")
       cat("Now we can add builtin indicators:\n\n")
       cat("Moving Average Convergence Divergence Indicator (from TTR)\n> addMACD()\n")
       plot(addMACD())
       readline("Press <Enter> to continue")
       cat("Add Bollinger Bands\n> addBBands()\n")
       plot(addBBands())
       readline("Press <Enter> to continue")
       cat("Drop Bollinger Bands\n> dropTA('BBands')\n")
       dropTA('BBands')
       readline("Press <Enter> to continue")
       cat("Zoom chart from full data to last 3 months\n> zoomChart(\"last 3 months\")\n")
       zoomChart('last 3 months')
       readline("Press <Enter> to continue")
       cat("Zoom back to full data\n> zoomChart()\n\n")
       zoomChart()
       rm(data)
     }
  #DATA######
  for(i in tickers){
    getSymbols(i,src="yahoo")
  }
  for(i in tickers.SHORTS){
    getSymbols(i,src="yahoo")
  }

getSymbols("^IXIC",scr="yahoo")
getSymbols("GOOG",src="yahoo")
getSymbols("AAPL",src="yahoo")

i=1
#"AAPL"
for(labels in labels.tech[2:18]){
  for(chart in charts[i]){
if(chart=="bar.chart"){
barChart(to.monthly(labels))}else{if(chart=="candle.chart"){
candleChart(to.weekly(labels),multi.col=TRUE,theme="white",up.col='green',dn.col='red')}else{chartSeries(to.weekly(labels),name=("AAPL"),up.col='white',dn.col='blue')}}
}}
addMACD()
addBBands()
i=1
tickers.name<-c("^NYA","MSFT","AAPL","GOOGL",#"EPAM",
                "FB","INTC","CSCO","TSM","ORCL","SAP","ADBE","IBM","CRM")
for(j in tickers.name){
for(chart in charts[i]){
  if(chart=="bar.chart"){
    barChart(get(paste(EPAM)))}else{if(chart=="candle.chart"){
      candleChart(get(paste(EPAM)),multi.col=TRUE,theme="white",up.col='green',dn.col='red')}else{chartSeries(get(paste(EPAM)),name=c("MSFT","AAPL","GOOGL","EPAM","FB","INTC","CSCO","TSM","ORCL","SAP","ADBE","IBM","CRM"),up.col='white',dn.col='blue')}}
  }
  addMACD(signal=9)
  addBBands()
  zoomChart('last 3 weeks')
  zoomChart()
}
Delt(EPAM$EPAM.Open)
plot(Delt(EPAM$EPAM.Close))

dir.create(paste(OneDrive,"H/Investing/data",sep=""))
save.image(paste(OneDrive,"H/Investing/data","/technology.RData",sep=""))
EXCEL.CAPITAL.MARKET<-cbind(`2357.TW`,BABA,AMZN,EPAM,MSFT,AAPL,GOOG,FB,INTC,CSCO,TSM,ORCL,SAP,ADBE,IBM,CRM,IXIC)
EXCEL.CAPITAL.MARKET<-data.frame(EXCEL.CAPITAL.MARKET)
NYSE<-data.frame(NYA)
WriteXLS::WriteXLS(EXCEL.CAPITAL.MARKET,"EXCEL.CAPITAL.MARKET and INDEX.xlsx", row.names = TRUE,col.names = TRUE,FreezeRow = 1,FreezeCol = 1)
WriteXLS::WriteXLS(NYSE,"INDEX.xlsx", row.names = TRUE,col.names = TRUE,FreezeRow = 1,FreezeCol = 1)

save.image(paste(wd,"/technology.RData",sep=""))
rm(list=ls())
q()
no
str.date(format(Sys.Date(),"YY%BB%"))-years(40)

