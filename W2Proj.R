#Libs
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)


myDL<-function(){
  temp<-tempfile()
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp, method="curl")
  unzip(temp)
  unlink(temp)
}

myTidy<-function(){
  #Read in the data, and make it tidy
  myData<<-fread("activity.csv", sep=",",header=TRUE)
  myData$date<<-ymd(myData$date)
  myDataNA<<-na.omit(myData)
  

}
myHisto<-function(){
  #Q2
  mySumData<<-na.omit(summarise(group_by(myData,date),steps=sum(steps)))
  print(ggplot(mySumData,aes(steps))+geom_histogram(bins=5))
}

myMM<-function(){
  #Q3
  #Mean and median number of steps taken each day
  myMMData<<-summarise(group_by(myDataNA,date), steps=sum(steps))
  print(mean(myMMData$steps))
  print(median(myMMData$steps))
}

myDailyAct<-function(){
  #Q4
  #Time series plot of the average number of steps taken
  myInterval<<-myDataNA %>%
    group_by(interval) %>%
    summarise(steps = sum(steps))
  
  g<-ggplot(myInterval,aes(interval,steps))+geom_line()
  print(g)
  #5
  myIntervalAvg<<-myDataNA %>%
    group_by(interval) %>%
    summarise(steps = mean(steps))
  
  print(myIntervalAvg[which.max(myIntervalAvg$steps),])
}
myImputtingNA<-function(){
  #Code to describe and show a strategy for imputing missing data
  #replace NA with another value, probably needs to use an apply of some sort
  #use lapply
  #myDataFix<<-myData[,stepsMean:=mean(steps), by=interval]
  myDataFix<<-myData
  myDataFix[,steps:=as.double(steps)]
  myDataFix[,stepsMean:=mean(steps,na.rm=TRUE), by=interval]
  myDataFix[is.na(steps),steps:=stepsMean]
}

#myDL()
myTidy()
#myHisto()
myMM()
myDailyAct()
myImputtingNA()