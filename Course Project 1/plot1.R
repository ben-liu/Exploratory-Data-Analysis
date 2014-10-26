# setwd("C:/Users/Ben/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1/")
# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1")

## Loading the data
source("data loading.R")

## Making Plots
# plot 1
png(filename = "plot1.png", width = 480, height = 480,units = "px", bg = "transparent")
hist(df$Global_active_power,col="red",main="Global Active Power",xlab="Global Active Power (kilowatts)")
dev.off()