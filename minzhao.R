if (!require(quantmod)) install.packages('quantmod')
if (!require(TTR)) install.packages('TTR')
if (!require(lubridate)) install.packages('lubridate')
if (!require(rvest)) install.packages('rvest')
library(quantmod)
library(TTR)
library(lubridate)
library(rvest)

################
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
SP500 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()
SP500 <- SP500[[1]]
Tix <- SP500$`Ticker symbol`

# test
Tix = "AAPL"

#############
i<-1
sma50<-c()
options(warn=-1)
for (name in Tix) {
  tryCatch({
    print(i)
    #Data include open, high, low, close, volume for each day.
    getSymbols(name,src="yahoo",from=(Sys.Date()-years(1)),to=Sys.Date())
    #use day low to calculate sma 50
    assign(paste("sma50.", name, sep = ""),SMA(get(name)[,3],50)) 
    b<-cbind(get(paste("sma50.", name, sep = "")),get(name)[,3])
    b$indicator<-b[,1]>b[,2]
    assign(paste("sma50.", name, sep = ""),b)
    sma50[i]<-paste("sma50.", name, sep = "")
    i<-i+1
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
options(warn=0)

#select via ma50
stock.sma50<- vector()
for (i in 1:length(sma50)) {
  tryCatch({
    row<-dim(get(sma50[i]))[1]
    if(  get(sma50[i])[row,3]==1&&sum(get(sma50[i])[((row-14):(row-1)),3])==0
    ){stock.sma50<-c(stock.sma50,Tix[i])}   
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

stock.sma50

for (name in stock.sma50) {
  chartSeries(get(name), subset='last 12 months')
  #candleChart(XRX,theme="black")
  addTA(stoch(HLC(get(name))),col=c(2,3,4))
  plot(addMACD())
  addSMA(20,col = "pink")
  addSMA(50,col = "blue")
  addSMA(200,col = "red")
  title(name)
}
