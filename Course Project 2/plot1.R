# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 2")

NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")

# 1.  Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, 
# make a plot showing the total PM2.5 emission from all sources for 
# each of the years 1999, 2002, 2005, and 2008.

x = with(NEI,aggregate(Emissions,by=list(year),sum))
names(x) = c("Year","Emissions")
with(x,plot(Year,Emissions,type="l",
            ylab = expression('Total PM'[2.5]*" Emission"),
            main="Total PM2.5 emission from 1999 to 2008")
            )
     )
dev.copy(png,file="plot1.png")
dev.off()

