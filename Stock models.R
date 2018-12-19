#//////////////////////////////////////////////////////////////////////////////////
## Load libraries #####
#//////////////////////////////////////////////////////////////////////////////////
rm(list=ls())


## My Alphavantage key is H0L95TLQP9HA1NTQ

setwd("D:/My Codes/Stocks/")
load("Current Stock Work.RData")

library(RCurl)
library(ggplot2)
library(plyr)
library(dplyr)
library(lubridate)
library(tidyr)
library(caret)
library(mxnet)
library(nnet)
library(RJSONIO)
library(Rmisc)
library(keras)


## avoid cientific notations
options(scipen=999)

## Set the proper environment
Sys.getenv()
## Install keras again
# install_keras(tensorflow = "gpu")

K <- backend()
const <- K$constant(42)
const

#//////////////////////////////////////////////////////////////////////////////////
## Downdload stock information #####
#//////////////////////////////////////////////////////////////////////////////////
dataInterval <- 600
lengthReadings <- 100000

stockNameList <- c("AAPL","AMZN","NVDA","NFLX","FB","BABA","ADBE","FDX","UNH","VNOM",
                   "MSFT","TSLA","WB","GOOGL","INTC","ALGN","ILMN","COST","BA","CTAS",
                   "IDXX","MCO","CSX","CAT","V","WMT","MCD","HD","VRTX","LRCX",
                   "ATVI","PYPL","CNC")

stockNameList <- unique(stockNameList)

finalStock <- matrix(nrow = 0, ncol = 11)
firstValueList <- list()
listIndex <- 1

numVars <- c("DateTime","Close","High","Low","Open","Volume", "Date","Date1","firstClose")

for (stockName in stockNameList){
  cat("1. Now doing Stock", stockName, "\n")
  ## Google download
  tmp <- getURL(paste("https://www.google.com/finance/getprices?i=", dataInterval, "&p=",
                      lengthReadings,"d&f=d,o,h,l,c,v&df=cpct&q=",stockName,sep = ""))
  tmp <- strsplit(tmp,'\n')
  tmp <- tmp[[1]]
  
  ## Remove the first not neccesary info
  tmp <- tmp[-c(1:7)]
  ## Separate the elements
  tmp <- strsplit(tmp,',')
  ## Join into a matrix
  tmp <- do.call('rbind',tmp)
  ## Repeat the first column
  tmp <- cbind(tmp,tmp[,1])
  ## Conver to data frame
  stockData <- data.frame(tmp)
  ## Assign names to columns
  colnames(stockData) <- c("DateTime","Close","High","Low","Open","Volume","TempVar")
  ## Create levels for each date in the dataframe
  stockData[,7] <- grepl(pattern = "a",x = stockData[,7])
  stockData$Date <- cumsum(stockData[,7])
  ## Remove the a from the date
  stockData$DateTime <- gsub(pattern = "a",replacement = "",x = stockData$DateTime)
  ## Assign the date to the rest of the readings that day
  stockData <- data.frame(stockData%>%
                            group_by(Date)%>%
                            mutate(Date1 = DateTime[1]))
  ## Add the Stock Name
  stockData$StockName <- stockName
  ## Delta from beginning of reading
  stockData$firstClose <- stockData$Close[1]
  firstValueList[listIndex] <- stockData$Close[1]
  names(firstValueList)[listIndex] <- stockName
  listIndex <- listIndex + 1
  
  ## Convert to matrix to make things quicker
  tempMatrix <- as.matrix(stockData)
  ## Add to the final matrix
  finalStock <- rbind(tempMatrix,finalStock)
  cat("2. DONE with", stockName, "\n")
}

## Convert to data frame
stockData <- as.data.frame(finalStock)

## Right format to each variable
stockData[,numVars] <- apply(stockData[,numVars], 2, function(x) as.numeric(as.character(x)))

## Assign the actual date time to each reading
stockData$TempVar <- stockData$TempVar == " TRUE"
stockData$TempVar <- !stockData$TempVar
stockData$DateTime <- stockData$DateTime*stockData$TempVar
stockData$DateTime <- as.POSIXct(stockData$DateTime*dataInterval+stockData$Date1,tz = "EST",origin = '1970-01-01')

## Remove unnecessary variables
stockData$TempVar <- NULL
stockData$Date <- NULL
stockData$unixTime <- stockData$Date1
stockData$Date1 <- NULL


## Add other important variables for periodic elements
stockData$timeMin <- hour(stockData$DateTime)*60+minute(stockData$DateTime)
stockData$DayOfWeek <-as.numeric(strftime(stockData$DateTime, format = "%w"))
stockData$WeekOfYear<-as.numeric(strftime(stockData$DateTime, format = "%W"))
stockData$DayOfYear <- yday(stockData$DateTime)

## Delta of closing
stockData$ClosingDelta <- (stockData$Close - stockData$firstClose)/stockData$firstClose

ggplot(data = stockData, aes(x = DateTime, y = ClosingDelta*100, group = StockName, color = StockName))+
  geom_point()+
  geom_line()


#//////////////////////////////////////////////////////////////////////////////////
## Download today or Last Days Stock Info  - GOOGLE #####
#//////////////////////////////////////////////////////////////////////////////////
lengthReadings <- 13

updateStock <- matrix(nrow = 0, ncol = 11)

for (stockName in stockNameList){
  cat("1. Now doing Stock", stockName, "\n")
  ## Google download
  tmp <- getURL(paste("https://www.google.com/finance/getprices?i=", dataInterval, "&p=",
                      lengthReadings,"d&f=d,o,h,l,c,v&df=cpct&q=",stockName,sep = ""))
  
  tmp <- getURL(paste("https://finance.yahoo.com/quote/AAPL/history?period1=1534111200&period2=1534197600&interval=1d&filter=history&frequency=1d",sep = ""))
  
  
  tmp <- strsplit(tmp,'\n')
  tmp <- tmp[[1]]
  
  ## Remove the first not neccesary info
  tmp <- tmp[-c(1:7)]
  ## Separate the elements
  tmp <- strsplit(tmp,',')
  ## Join into a matrix
  tmp <- do.call('rbind',tmp)
  ## Repeat the first column
  tmp <- cbind(tmp,tmp[,1])
  ## Conver to data frame
  tempData <- data.frame(tmp)
  ## Assign names to columns
  colnames(tempData) <- c("DateTime","Close","High","Low","Open","Volume","TempVar")
  ## Create levels for each date in the dataframe
  tempData[,7] <- grepl(pattern = "a",x = tempData[,7])
  tempData$Date <- cumsum(tempData[,7])
  ## Remove the a from the date
  tempData$DateTime <- gsub(pattern = "a",replacement = "",x = tempData$DateTime)
  ## Assign the date to the rest of the readings that day
  tempData <- data.frame(tempData%>%
                            group_by(Date)%>%
                            mutate(Date1 = DateTime[1]))
  ## Add the Stock Name
  tempData$StockName <- stockName
  ## Delta from beginning of reading
  tempData$firstClose <- stockData[stockData$StockName == stockName,"firstClose"][1]
  
  ## Convert to matrix to make things quicker
  tempMatrix <- as.matrix(tempData)
  ## Add to the final matrix
  updateStock <- rbind(tempMatrix,updateStock)
  cat("2. DONE with", stockName, "\n")
}

## Convert to data frame
updateStock <- as.data.frame(updateStock)

## Right format to each variable
updateStock[,numVars] <- apply(updateStock[,numVars], 2, function(x) as.numeric(as.character(x)))

## Assign the actual date time to each reading
updateStock$TempVar <- updateStock$TempVar == " TRUE"
updateStock$TempVar <- !updateStock$TempVar
updateStock$DateTime <- updateStock$DateTime*updateStock$TempVar
updateStock$DateTime <- as.POSIXct(updateStock$DateTime*dataInterval+updateStock$Date1,tz = "EST",origin = '1970-01-01')

## Remove unnecessary variables
updateStock$TempVar <- NULL
updateStock$Date <- NULL
updateStock$unixTime <- updateStock$Date1
updateStock$Date1 <- NULL


## Add other important variables for periodic elements
updateStock$timeMin <- hour(updateStock$DateTime)*60+minute(updateStock$DateTime)
updateStock$DayOfWeek <-as.numeric(strftime(updateStock$DateTime, format = "%w"))
updateStock$WeekOfYear<-as.numeric(strftime(updateStock$DateTime, format = "%W"))
updateStock$DayOfYear <- yday(updateStock$DateTime)

## Delta of closing
updateStock$ClosingDelta <- (updateStock$Close - updateStock$firstClose)/updateStock$firstClose

## Check the current update
ggplot(data = updateStock, aes(x = DateTime, y = ClosingDelta*100, group = StockName, color = StockName))+
  geom_point()+
  geom_line()

## Add update to the old data
stockData <- rbind(stockData, updateStock)

## Check the total history of stocks
ggplot(data = stockData, aes(x = DateTime, y = ClosingDelta*100, group = StockName, color = StockName))+
  geom_point()+
  geom_line()

#//////////////////////////////////////////////////////////////////////////////////
## Download today or Last Days Stock Info  - IEX #####
#//////////////////////////////////////////////////////////////////////////////////
updateStock <- data.frame()

for (stockName in stockNameList){
  cat("1. Now doing Stock", stockName, "\n")
  ## iex download
  tmp <- fromJSON(paste("https://api.iextrading.com/1.0/stock/",stockName,"/chart/1d?chartInterval=10",sep = ""))
  tmp <- lapply(tmp, function(x) {
    x[sapply(x, is.null)] <- NA
    unlist(x)
  })
  tmp <- do.call("rbind.fill", lapply(tmp, as.data.frame(t)))
  
  ## Transform date and minute to DateTime
  tmp$DateTime <- paste(tmp$value.date,tmp$value.minute, sep = " ")
  tmp$DateTime <- as.POSIXct(tmp$DateTime, format = "%Y%m%d %H:%M",tz = "EST",origin = '1970-01-01')
  ## Remove the date, minite and label columns
  tmp$value.date <- NULL
  tmp$value.minute <- NULL
  tmp$value.label <- NULL

  ## Add the Stock Name
  tmp$StockName <- stockName
  ## Delta from beginning of reading
  tmp$firstClose <- stockData[stockData$StockName == stockName,"firstClose"][1]
  
  ## Add to the final matrix
  updateStock <- rbind.fill(tmp,updateStock)
  cat("2. DONE with", stockName, "\n")
}

## Assign names to columns
colnames(tmp) <- c("high","low","average","Volume","notional","numberOfTrades","marketHigh",
                   "marketLow","marketAverage","marketVolume","marketNotional","marketNumberOfTrades",
                   "Open","Close","marketOpen","marketClose","changeOverTime","marketChangeOverTime",
                   "DateTime")


## Fill in the data of market High and market Low with high and low

## Convert to data frame
updateStock <- as.data.frame(updateStock)

## Right format to each variable
updateStock[,numVars] <- apply(updateStock[,numVars], 2, function(x) as.numeric(as.character(x)))

## Add other important variables for periodic elements
updateStock$timeMin <- hour(updateStock$DateTime)*60+minute(updateStock$DateTime)
updateStock$DayOfWeek <-as.numeric(strftime(updateStock$DateTime, format = "%w"))
updateStock$WeekOfYear<-as.numeric(strftime(updateStock$DateTime, format = "%W"))
updateStock$DayOfYear <- yday(updateStock$DateTime)

## Delta of closing
updateStock$ClosingDelta <- (updateStock$Close - updateStock$firstClose)/updateStock$firstClose

## Check the current update
ggplot(data = updateStock, aes(x = DateTime, y = ClosingDelta*100, group = StockName, color = StockName))+
  geom_point()+
  geom_line()

## Add update to the old data
stockData <- rbind(stockData, updateStock)

## Check the total history of stocks
ggplot(data = stockData, aes(x = DateTime, y = ClosingDelta*100, group = StockName, color = StockName))+
  geom_point()+
  geom_line()

#//////////////////////////////////////////////////////////////////////////////////
## Download today or Last Days Stock Info  - Alphavantage #####
#//////////////////////////////////////////////////////////////////////////////////
## Check the last date on the table
max(stockData$DateTime)
## Create the data frame to save the updates
updateStock <- data.frame()
## Run the uupdate function on each stock on the list
for (stockName in stockNameList){
  cat("1. Now doing Stock", stockName, "\n")
  ## iex download
  tmp <- read.csv(paste("https://www.alphavantage.co/query?function=TIME_SERIES_INTRADAY&symbol=",stockName,
                        "&interval=5min&outputsize=full&apikey=H0L95TLQP9HA1NTQ&datatype=csv",sep = ""))
                
  ## Add the Stock Name
  tmp$StockName <- stockName
  ## Delta from beginning of reading
  tmp$firstClose <- stockData[stockData$StockName == stockName,"firstClose"][1]
  
  ## Add to the final matrix
  updateStock <- rbind.fill(tmp,updateStock)
  cat("2. DONE with", stockName, "\n")
  Sys.sleep(15)
}

## Transform date and minute to DateTime
updateStock$DateTime <- as.POSIXct(updateStock$timestamp, format = "%Y-%m-%d %H:%M:%S",tz = "EST",origin = '1970-01-01')
updateStock$timestamp <- NULL

## Assign names to columns
colnames(updateStock) <- c("Open","High","Low","Close","Volume","StockName","firstClose","DateTime")

## Add other important variables for periodic elements
updateStock$timeMin <- hour(updateStock$DateTime)*60+minute(updateStock$DateTime)
updateStock$DayOfWeek <-as.numeric(strftime(updateStock$DateTime, format = "%w"))
updateStock$WeekOfYear<-as.numeric(strftime(updateStock$DateTime, format = "%W"))
updateStock$DayOfYear <- yday(updateStock$DateTime)
updateStock$unixTime <- as.numeric(updateStock$DateTime)

## Delta of closing
updateStock$ClosingDelta <- (updateStock$Close - updateStock$firstClose)/updateStock$firstClose

## Check the current update
ggplot(data = updateStock, aes(x = DateTime, y = ClosingDelta*100, group = StockName, color = StockName))+
  geom_point()+
  geom_line()

## Reorder Columns of dataframe
updateStock <- updateStock[,colnames(stockData)]

## Filter out updates that are already in the mother of all stocks table
updateStock <- updateStock[updateStock$DateTime > max(stockData$DateTime),]

## Add update to the old data
stockData <- rbind(stockData, updateStock)

## Order Stockdata by date
stockData <- stockData[order(stockData$DateTime,decreasing = FALSE),]

stockData <- stockData[!duplicated(stockData[,c("DateTime","StockName")]),]

## Check the total history of stocks
ggplot(data = stockData, aes(x = DateTime, y = ClosingDelta*100, group = StockName, color = StockName))+
  geom_point()+
  geom_line()

#//////////////////////////////////////////////////////////////////////////////////
## Pre process data  #####
#//////////////////////////////////////////////////////////////////////////////////
## Keep readings for the full seconds only
# readings_5Min <- as.numeric(format(stockData$DateTime, "%M"))
# minutes_to_remove <- c(5,15,25,35,45,55)

## Remove the radings that are at minute 5, 15... etc
# modelData <- stockData[!readings_5Min %in% minutes_to_remove,]
tail(stockData,50)

## Use all readings
modelData <- stockData

## FUNCTIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## For the NA values copy the value of the previous time
NAFilling <- function(df, variableNumber){
  df[,variableNumber] <- ifelse(is.na(df[,variableNumber]),df[,variableNumber-1],df[,variableNumber])  
  return(df)
}

## Function to create the data Frames (independent window and response window)
CreateDataFrames <- function(BeginningValue, HistoryWindow, FutureWindow){
  trainingData<- datNNwide[,c((BeginningValue+1):(HistoryWindow+FutureWindow+BeginningValue))]
  ## Fill the NAs from the training Dataset
  for(i in 3:ncol(trainingData)){
    trainingData <- NAFilling(df = trainingData,variableNumber = i)
  }
  ## Set the x and y datasets
  data.x <- data.matrix(trainingData[,c((1):HistoryWindow)])
  data.y <- data.matrix(trainingData[,c((HistoryWindow+1):(HistoryWindow+FutureWindow))])
  
  ## Rename variables to be able to predict in the future as well
  colnames(data.x) <- paste("time-",c((HistoryWindow-1):0)*dataInterval, sep = "")
  colnames(data.y) <- paste("time+",c(1:FutureWindow)*dataInterval, sep = "")
  
  dataframes <- list()
  dataframes$data.x <- data.x
  dataframes$data.y <- data.y
  
  return(dataframes)
}

## Root Mean squared error
Rmse <- function(target, response) {
  return(sqrt(mean((target - response)^2)))
}
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Determine how far in the future we can forecast
futureWindow <- 40
## Determine Shift of the testing data
testingShift <- 20
## Determine the number of folds
foldsNumber <- 5

## Reduce the data to the historic values only
datNN <- modelData[,c("StockName","DateTime","ClosingDelta")]

## Long to wide
datNNwide <- data.frame(datNN%>%
                          group_by(StockName)%>%
                          spread(key = DateTime,value = ClosingDelta))


## First reading are all supposed to be zero considering there was no change
datNNwide[,2] <- rep(0,nrow(datNNwide))

#//////////////////////////////////////////////////////////////////////////////////
## DNN MXNET #####
#//////////////////////////////////////////////////////////////////////////////////
## Set the hyperparameters to test out
dnnHyperpGrid <- expand.grid(historyWindow <- c(2000), 
                             nRoundList <- c(1000),
                             batchSizeList <- c(100),
                             optimizerList <- "rmsprop",
                             activattionTypeList <- c("tanh"),
                             numHidden1List <- c(100,200,300),
                             numHidden2List <- c(100,200,300),
                             numHidden3List <- c(100,200,300),
                             learnRate <- c(.07))
colnames(dnnHyperpGrid) <- c("History_Window",
                             "Number_Of_Rounds",
                             "Batch_Size",
                             "Optimizer",
                             "Activation_type",
                             "Num_Hidden_1",
                             "Num_Hidden_2",
                             "Num_Hidden_3",
                             "Learn_Rate")

matrixNames <-  c("History_Window",
                  "Future_Window",
                  "Testing_Shift",
                  "Number_Of_Rounds",
                  "Batch_Size",
                  "Optimizer",
                  "Activation_type",
                  "Num_Hidden_1",
                  "Num_Hidden_2",
                  "Num_Hidden_3",
                  "Learn_Rate",
                  "Fold_Number",
                  "RMSE",
                  "RSq")
dnnMatrixResults <- matrix(nrow = nrow(dnnHyperpGrid)*foldsNumber,
                           ncol = length(matrixNames))
colnames(dnnMatrixResults) <-matrixNames

## Star the rowCounter
rowCounter <- 1

## Explore the hyperparameters
for (combiNumber in 1:nrow(dnnHyperpGrid)){
  ## Determine the step size for each fold
  stepSize <- floor((ncol(datNNwide)-1-dnnHyperpGrid$History_Window[combiNumber]-futureWindow-testingShift)/foldsNumber)
  ## We start the cross validation loop here.
  for (fold in 1:foldsNumber){
    cat("1. Now doing fold ", fold, "\n")
    
    ## Determine the start of the story
    beginningValue <- ((fold-1)*stepSize)+1
    
    ## Define structure of the DNN
    data <- mx.symbol.Variable("data")
    fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=dnnHyperpGrid$Num_Hidden_1[combiNumber])
    act1 <- mx.symbol.Activation(fc1,act.type=as.character(dnnHyperpGrid$Activation_type[combiNumber]))
    fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=dnnHyperpGrid$Num_Hidden_2[combiNumber])
    act2 <- mx.symbol.Activation(fc2,act.type=as.character(dnnHyperpGrid$Activation_type[combiNumber]))
    fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=dnnHyperpGrid$Num_Hidden_3[combiNumber])
    act3 <- mx.symbol.Activation(fc3,act.type=as.character(dnnHyperpGrid$Activation_type[combiNumber]))
    fc4 <- mx.symbol.FullyConnected(act3, name="fc4", num_hidden=futureWindow)
    lro1 <- mx.symbol.LinearRegressionOutput(data=fc4, name="lro")
    
    ## Training Dataset and testing Dataset
    trainingData <- CreateDataFrames(BeginningValue = beginningValue,
                                     HistoryWindow = dnnHyperpGrid$History_Window[combiNumber],
                                     FutureWindow = futureWindow)
    
    testingData <- CreateDataFrames(BeginningValue = beginningValue + testingShift,
                                    HistoryWindow = dnnHyperpGrid$History_Window[combiNumber],
                                    FutureWindow = futureWindow)
    
    ## Convert training and testing to proper structure for the DNN
    train_iter <- mx.io.arrayiter(data = t(trainingData$data.x), label = t(trainingData$data.y))
    test_iter <- mx.io.arrayiter(data = t(testingData$data.x), label = t(testingData$data.y))
    
    ## Train the DNN 
    dnnFit <- mx.model.FeedForward.create(lro1, X=train_iter,
                                          eval.data = test_iter,
                                          ctx=mx.gpu(), 
                                          num.round=dnnHyperpGrid$Number_Of_Rounds[combiNumber], 
                                          array.batch.size=dnnHyperpGrid$Batch_Size[combiNumber],
                                          array.layout = "rowmajor",
                                          verbose = FALSE,
                                          learning.rate=dnnHyperpGrid$Learn_Rate[combiNumber],
                                          # learning.rate=.1,
                                          optimizer = as.character(dnnHyperpGrid$Optimizer[combiNumber]))
    ## Create the predictions
    testingPredictions <- t(predict(dnnFit, testingData$data.x,array.layout = "rowmajor"))
    
    ## Check the metrics, Rmse and RSq
    dnnMatrixResults[rowCounter,] <- c(dnnHyperpGrid$History_Window[combiNumber],
                                       futureWindow,
                                       testingShift,
                                       dnnHyperpGrid$Number_Of_Rounds[combiNumber],
                                       dnnHyperpGrid$Batch_Size[combiNumber],
                                       as.character(dnnHyperpGrid$Optimizer[combiNumber]),
                                       as.character(dnnHyperpGrid$Activation_type[combiNumber]),
                                       dnnHyperpGrid$Num_Hidden_1[combiNumber],
                                       dnnHyperpGrid$Num_Hidden_2[combiNumber],
                                       dnnHyperpGrid$Num_Hidden_3[combiNumber],
                                       dnnHyperpGrid$Learn_Rate[combiNumber],
                                       fold,
                                       Rmse(testingData$data.y, testingPredictions),
                                       1 - (sum((testingData$data.y-testingPredictions )^2)/sum((testingData$data.y-mean(testingData$data.y))^2)))
    
  
  ## Calculate percetage done
  percentageDone <- (rowCounter/nrow(dnnMatrixResults))*100
  
  
  cat("2. Done with fold ", fold, "\n")
  cat("------ Progress: ", percentageDone, "%", "\n")
  
  ## Add to the row Counter
  rowCounter <- rowCounter+1
}
}

## Format the outpput to graph it
head(dnnMatrixResults)
dnnResultsDat <- as.data.frame(dnnMatrixResults)
head(dnnResultsDat)
summary(dnnResultsDat)
numVars <- c("RMSE",
             "RSq")
dnnResultsDat[numVars] <- data.frame(lapply(dnnResultsDat[numVars] , function(x) as.numeric(as.character(x))))

## Get the summary for each category
MSE <- summarySE(data = dnnResultsDat,measurevar = "RSq",groupvars = c("History_Window",
                                                                       "Future_Window",
                                                                       "Testing_Shift",
                                                                       "Optimizer",
                                                                       "Activation_type",
                                                                       "Number_Of_Rounds",
                                                                       "Batch_Size",
                                                                       "Num_Hidden_1",
                                                                       "Num_Hidden_2",
                                                                       "Num_Hidden_3",
                                                                       "Learn_Rate"),na.rm = TRUE)
str(MSE)

head(MSE[order(MSE$RSq, decreasing = TRUE),],20)

## Save best model so far
bestModelSofar <- head(MSE[order(MSE$RSq, decreasing = TRUE),],20)

## Graph the performance of the models
ggplot(data = MSE[MSE$Number_Of_Rounds != "100",],aes(x = Num_Hidden_1, y = RSq, color = Num_Hidden_2, group = Num_Hidden_2))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin = RSq - ci, ymax = RSq + ci))+
  facet_grid(Number_Of_Rounds~Batch_Size)


## Graph the results of the model
testingGraph <- data.frame(datNNwide[,c(1,(beginningValue+dnnHyperpGrid$History_Window[2]+testingShift+1):
                                          (beginningValue+dnnHyperpGrid$History_Window[1]+futureWindow+testingShift))]%>%
                             gather(key = DateTime,value = ClosingDelta,... = 2:(futureWindow+1)))

testingGraph$dnnPredictions <- as.vector(testingPredictions)
testingGraph$DateTime <- gsub(pattern = "X",replacement = "",x = testingGraph$DateTime )
testingGraph$DateTime <- as.POSIXct(x = testingGraph$DateTime, format = "%Y.%m.%d.%H.%M.%OS")
head(testingGraph)

ggplot(data = testingGraph, aes(x = DateTime, group = StockName, color = StockName))+
  geom_point(aes(y = ClosingDelta), shape = 19)+
  geom_line(aes(y = ClosingDelta))+
  geom_point(aes(y = dnnPredictions),shape = 25)+
  geom_line(aes(y = dnnPredictions))

#//////////////////////////////////////////////////////////////////////////////////
## DNN with Keras #####
#//////////////////////////////////////////////////////////////////////////////////
## Set the hyperparameters to test out
dnnHyperpGridKeras <- expand.grid(historyWindow <- c(2000,3000), 
                             nRoundList <- c(2000),
                             batchSizeList <- c(100),
                             optimizerList <- c("rmsprop"),
                             activattionTypeList <- c("tanh"),
                             numHidden1List <- c(100,50),
                             numHidden2List <- c(100,50),
                             numHidden3List <- c(100,50),
                             numHidden4List <- c(100,50,10),
                             learnRate <- c(.0001))
colnames(dnnHyperpGridKeras) <- c("History_Window",
                             "Number_Of_Rounds",
                             "Batch_Size",
                             "Optimizer",
                             "Activation_type",
                             "Num_Hidden_1",
                             "Num_Hidden_2",
                             "Num_Hidden_3",
                             "Num_Hidden_4",
                             "Learn_Rate")

matrixNames <-  c("History_Window",
                  "Future_Window",
                  "Testing_Shift",
                  "Number_Of_Rounds",
                  "Batch_Size",
                  "Optimizer",
                  "Activation_type",
                  "Num_Hidden_1",
                  "Num_Hidden_2",
                  "Num_Hidden_3",
                  "Num_Hidden_4",
                  "Learn_Rate",
                  "Fold_Number",
                  "RMSE",
                  "RSq")
dnnMatrixResults <- matrix(nrow = nrow(dnnHyperpGridKeras)*foldsNumber,
                           ncol = length(matrixNames))
colnames(dnnMatrixResults) <-matrixNames


## Star the rowCounter
rowCounter <- 1
## Explore the hyperparameters
for (combiNumber in 1:nrow(dnnHyperpGridKeras)){
  ## Determine the step size for each fold
  stepSize <- floor((ncol(datNNwide)-1-dnnHyperpGridKeras$History_Window[combiNumber]-futureWindow-testingShift)/foldsNumber)
## We start the cross validation loop here.
for (fold in 1:foldsNumber){
  cat("1. Now doing fold ", fold, "\n")
  
  ## Determine the start of the story
  beginningValue <- ((fold-1)*stepSize)+1
  
  ## Define structure of the DNN
  model <- keras_model_sequential() 
  model %>% 
    layer_dense(units = dnnHyperpGridKeras$Num_Hidden_1[combiNumber], input_shape = c(dnnHyperpGridKeras$History_Window[combiNumber]),
                activation = dnnHyperpGridKeras$Activation_type[combiNumber],kernel_initializer = "normal") %>% 
    layer_dense(units = dnnHyperpGridKeras$Num_Hidden_2[combiNumber], 
                activation = dnnHyperpGridKeras$Activation_type[combiNumber], kernel_initializer='normal') %>%
    layer_dense(units = dnnHyperpGridKeras$Num_Hidden_3[combiNumber], 
                activation = dnnHyperpGridKeras$Activation_type[combiNumber], kernel_initializer='normal') %>%
    layer_dense(units = dnnHyperpGridKeras$Num_Hidden_4[combiNumber], 
                activation = dnnHyperpGridKeras$Activation_type[combiNumber], kernel_initializer='normal') %>%
    layer_dense(units = futureWindow, activation = dnnHyperpGridKeras$Activation_type[combiNumber], kernel_initializer='normal')
  
  # summary(model)
  
  ## Training Dataset and testing Dataset
  trainingData <- CreateDataFrames(BeginningValue = beginningValue,
                                   HistoryWindow = dnnHyperpGridKeras$History_Window[combiNumber],
                                   FutureWindow = futureWindow)
  
  testingData <- CreateDataFrames(BeginningValue = beginningValue + testingShift,
                                  HistoryWindow = dnnHyperpGridKeras$History_Window[combiNumber],
                                  FutureWindow = futureWindow)
  
  ## Convert training and testing to proper structure for the DNN
  x_train <- array_reshape(x = trainingData$data.x, dim =  c(nrow(trainingData$data.x), dnnHyperpGridKeras$History_Window[combiNumber]))
  x_test <- array_reshape(x = testingData$data.x, c(nrow(testingData$data.x), dnnHyperpGridKeras$History_Window[combiNumber]))
  
  y_train <- as.matrix(trainingData$data.y)
  y_test <- as.matrix(testingData$data.y)
  
  ## Train the model
  
  model %>% compile(
    optimizer = optimizer_rmsprop(lr = dnnHyperpGridKeras$Learn_Rate[combiNumber]),
    loss = 'mse')
  
  model %>% fit(x_train, 
                y_train,
                epochs=dnnHyperpGridKeras$Number_Of_Rounds[combiNumber],
                batch_size=dnnHyperpGridKeras$Batch_Size[combiNumber],
                verbose = 1)
  
  ## Evaluate the model
  score <- model %>% evaluate(x_test, 
                              y_test,
                              batch_size = dnnHyperpGridKeras$Batch_Size[combiNumber])
  
  
  ## Create the predictions
  testingPredictions <- predict(model, x_test,verbose = 0)
  
  ## Check the metrics, Rmse and RSq
  dnnMatrixResults[rowCounter,] <- c(dnnHyperpGridKeras$History_Window[combiNumber],
                                     futureWindow,
                                     testingShift,
                                     dnnHyperpGridKeras$Number_Of_Rounds[combiNumber],
                                     dnnHyperpGridKeras$Batch_Size[combiNumber],
                                     as.character(dnnHyperpGridKeras$Optimizer[combiNumber]),
                                     as.character(dnnHyperpGridKeras$Activation_type[combiNumber]),
                                     dnnHyperpGridKeras$Num_Hidden_1[combiNumber],
                                     dnnHyperpGridKeras$Num_Hidden_2[combiNumber],
                                     dnnHyperpGridKeras$Num_Hidden_3[combiNumber],
                                     dnnHyperpGridKeras$Num_Hidden_4[combiNumber],
                                     dnnHyperpGridKeras$Learn_Rate[combiNumber],
                                     fold,
                                     as.numeric(sqrt(score)),
                                     1 - (sum((testingData$data.y-testingPredictions )^2)/sum((testingData$data.y-mean(testingData$data.y))^2)))
  
  ## Calculate percetage done
  percentageDone <- (rowCounter/nrow(dnnMatrixResults))*100
  
  
  cat("2. Done with fold ", fold, "\n")
  cat("------ Progress: ", percentageDone, "%", "\n")
  
  ## Add to the row Counter
  rowCounter <- rowCounter+1
  
}
}

## Format the outpput to graph it
head(dnnMatrixResults)
dnnResultsDat <- as.data.frame(dnnMatrixResults)
head(dnnResultsDat)
summary(dnnResultsDat)
numVars <- c("RMSE",
             "RSq")
dnnResultsDat[numVars] <- data.frame(lapply(dnnResultsDat[numVars] , function(x) as.numeric(as.character(x))))

## Get the summary for each category
MSE <- summarySE(data = dnnResultsDat,measurevar = "RSq",groupvars = c("History_Window",
                                                                       "Future_Window",
                                                                       "Testing_Shift",
                                                                       "Optimizer",
                                                                       "Activation_type",
                                                                       "Number_Of_Rounds",
                                                                       "Batch_Size",
                                                                       "Num_Hidden_1",
                                                                       "Num_Hidden_2",
                                                                       "Num_Hidden_3",
                                                                       "Num_Hidden_4",
                                                                       "Learn_Rate"),na.rm = TRUE)
str(MSE)

head(MSE[order(MSE$RSq, decreasing = TRUE),],20)
tail(MSE[order(MSE$RSq, decreasing = TRUE),],20)

#//////////////////////////////////////////////////////////////////////////////////
## LSTM with Keras #####
#//////////////////////////////////////////////////////////////////////////////////
n_timesteps_train <- 40
n_timesteps_test <- 4
n_predictions <- 1
batch_size_lstm <- 4
epochs_lstm <- 300

## Create the array that will hold the x and y arrays for the lstm
lstm_array <- array(0,dim = c(batch_size_lstm + n_predictions,
                              n_timesteps_train + n_timesteps_test,
                          length(stockNameList)))


for (batch in 1:dim(lstm_array)[1]+1){
  ## Training Dataset and testing Dataset
  trainingData_lstm <- CreateDataFrames(BeginningValue = batch,
                                        HistoryWindow = n_timesteps_train + n_timesteps_test,
                                        FutureWindow = 1)
  
  ## add the dataset to the array
  lstm_array[batch-1,,] <-t(trainingData_lstm$data.x)
}

lstm_x_array <- lstm_array[1:batch_size_lstm,1:n_timesteps_train,]
lstm_y_array <- lstm_array[2:(batch_size_lstm+1),41:44,2]

## Define the model
model <- keras_model_sequential()

model %>%
  layer_lstm(units            = 20, 
             input_shape      = c(n_timesteps_train, length(stockNameList)), 
             batch_size       = batch_size_lstm,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 20, 
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 4)

model %>% 
  compile(loss = 'mse', optimizer = 'adam')

model

for (i in 1:epochs_lstm) {
  model %>% fit(x          = lstm_x_array, 
                y          = lstm_y_array, 
                batch_size = batch_size_lstm,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
}

pred_out <- model %>% 
  predict(lstm_x_array, batch_size = batch_size_lstm)



#//////////////////////////////////////////////////////////////////////////////////
## Save current State of work #####
#//////////////////////////////////////////////////////////////////////////////////
rm("tmp","tempMatrix","finalStock")
gc()


save.image("Current Stock Work.RData")

