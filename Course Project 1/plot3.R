# setwd("C:/Users/Ben/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1/")
# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1")

## Loading the data
source("data loading.R")

## Making Plots
# plot 3
png(filename = "plot3.png", width = 480, height = 480,units = "px", bg = "transparent")

plot(df$Date.Time,df$Sub_metering_1,col="black",type="l",xlab="",ylab="Energy sub metering")
lines(df$Date.Time,df$Sub_metering_2,col="red")
lines(df$Date.Time,df$Sub_metering_3,col="blue")
legend("topright", col = c("black", "red", "blue"),c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),lwd = 1)

dev.off()


