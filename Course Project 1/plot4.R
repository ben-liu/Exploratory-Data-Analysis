# setwd("C:/Users/Ben/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1/")
# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 1")

## Loading the data
source("data loading.R")

## Making Plots
# plot 4
png(filename = "plot4.png", width = 500, height = 500,units = "px", bg = "transparent")
par(mfrow = c(2, 2))

attach(df)
plot(Date.Time, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")

plot(Date.Time, Voltage, type = "l", xlab = "Date.Time", ylab = "Voltage")

plot(Date.Time, Sub_metering_1, type = "l", col = "black", xlab = "", ylab = "Energy sub metering")
lines(Date.Time, Sub_metering_2, col = "red")
lines(Date.Time, Sub_metering_3, col = "blue")
legend("topright", bty = "n", col = c("black", "red", "blue"), c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lwd = 1)

plot(Date.Time, Global_reactive_power, type = "l", col = "black", xlab = "datetime", ylab = "Global_reactive_power")

dev.off()



