# setwd("C:/Users/Ben/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1/")

## Loading the data
# unzip file
unzip("exdata-data-household_power_consumption.zip")
file="household_power_consumption.txt"

#inspect dataset structure
read.table(file,nrow=5,sep=";",head=T) 

# extract relevant dataset 
# data is recorded on 1 minute interval
# skip rows from 2006-12-16 17:24 to 2007-01-31 23:59
library(lubridate)
datediff = as.numeric(as.Date("2007-01-31")-as.Date("2006-12-16"))*24*60 
timediff = hms("24:00:00")-hms("17:24:00")
mindiff = hour(timediff)*60+minute(timediff) + datediff
interval = 60*24*2

header<- names(read.table(file,nrow=1,sep=";",head=T))
df<- read.table(file,skip=mindiff+1,nrow=interval,sep=";",head=F,colClasses = c("character", "character", rep("numeric",7)),na = "?")
names(df)<- header

df$Date.Time<- strptime(paste(df$Date,df$Time),format="%d/%m/%Y %H:%M:%S")

# save the new dataset to save some disk place
# write.csv(df,"project1.csv",row.names=F)

