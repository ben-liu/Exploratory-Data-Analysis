# setwd("C:/Users/Ben/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1/")
# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1")

## Loading the data
source("data loading.R")

## Making Plots
# plot 2
png(filename = "plot2.png", width = 480, height = 480,units = "px", bg = "transparent")
with(df,plot(Date.Time,Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)"))
dev.off()

