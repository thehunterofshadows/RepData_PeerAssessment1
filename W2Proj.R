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
  #median is 0, so should you factor out times where steps are 0?  seems like that is pulling it down
  myMMData<<-summarise(group_by(myDataNA,date),mean=mean(steps),median=median(steps, na.rm=TRUE), sum=sum(steps))
}


#myDL()
myTidy()
myHisto()
myMM()