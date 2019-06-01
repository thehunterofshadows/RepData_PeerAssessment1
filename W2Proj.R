#Libs
library(data.table)
library(ggplot2)
library(dplyr)
library(lubridate)
library(timeDate)


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
  png("q2Plot.png",width=480,height=480)
    print(ggplot(mySumData,aes(steps))+geom_histogram())
  dev.off()
}

myMM<-function(){
  #Q3
  #Mean and median number of steps taken each day
  myMMData<<-summarise(group_by(myDataNA,date), steps=sum(steps))
  print(data.table(mean=mean(myMMData$steps),median=median(myMMData$steps)))
  
}

myDailyAct<-function(){
  #Q4
  #Time series plot of the average number of steps taken
  myInterval<<-myDataNA %>%
    group_by(interval) %>%
    summarise(steps = sum(steps))
  png("q4Plot.png",width=480,height=480)
  g<-ggplot(myInterval,aes(interval,steps))+geom_line()
  print(g)
  dev.off()
  #5
  myIntervalAvg<<-myDataNA %>%
    group_by(interval) %>%
    summarise(steps = mean(steps))
  
  print(myIntervalAvg[which.max(myIntervalAvg$steps),])
}
myImputtingNA<-function(){
  #Q6
  #Code to describe and show a strategy for imputing missing data
  #replace NA with another value, probably needs to use an apply of some sort
  #use lapply
  #myDataFix<<-myData[,stepsMean:=mean(steps), by=interval]
  myDataFix<<-myData
  #myDataFix[,steps:=as.double(steps)]
  #myDataFix[,stepsMean:=mean(steps,na.rm=TRUE), by=interval]
  myDataFix[,stepsMean:=mean(steps,na.rm=TRUE), by=interval]
  myDataFix[is.na(steps),steps:=stepsMean]
  
  #Q7 Histogram of the total number of steps taken each day after missing values are imputed
  mySumData2<<-summarise(group_by(myData,date),steps=sum(steps))
  png("q7Plot.png",width=480,height=480)
  print(ggplot(mySumData2,aes(steps))+geom_histogram())
  dev.off()
  print(data.table(mean=mean(mySumData2$steps),median=median(mySumData2$steps)))
  fwrite(myDataFix[,1:3],file="fixedData.csv")
}

myPanel<-function(){
  #use something like wday() to determien day of week
  #add that to the table, then us gg plot to create multiplot using that column
  #use this isWeekday(myData$date,wday=1:5)
  #mySumData2<<-data.table(mySumData2)
  #mySumData2[,wkdy:=isWeekday(date,wday=1:5)]
  #png("q8Plot.png",width=480,height=480)
  #g<-ggplot(mySumData2,aes(steps))
  #g<-g+facet_grid(wkdy ~.)
  #g<-g+geom_point()
  #print(g)
  #dev.off()
  
  myWeek<<-data.table(copy(myDataFix))
  myWeek[,wkdy:=isWeekday(date,wday=1:5)]
  myWeek[wkdy==TRUE,wkdy2:="Weekday"]
  myWeek[wkdy==FALSE,wkdy2:="Weekend"]
  myWeek<<-summarise(group_by(myWeek,wkdy2,interval),steps=mean(steps))
  #png("q8Plot.png",width=480,height=480)
  g<-ggplot(myWeek,aes(interval,steps))
  g<-g+facet_grid(wkdy2 ~.)
  g<-g+geom_line()
  print(g)
  #dev.off()
}

#myDL()
myTidy()
myHisto()
myMM()
myDailyAct()
myImputtingNA()
myPanel()