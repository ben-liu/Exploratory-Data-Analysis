# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 2")

NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")

# 2.	Have total emissions from PM2.5 decreased in the 
# Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the baseplotting system to make a plot answering this question.

bal= NEI[NEI$fips %in% "24510",]
x = with(bal,aggregate(Emissions,by=list(year),sum))
names(x) = c("Year","Emissions")

png(file="plot2.png")
par(mar=c(5,5,4,2)+0.1)
with(x,plot(Year,Emissions,type="l",
            ylab = expression('Total PM'[2.5]*" Emission"),
            main="Total PM2.5 emission in \n Baltimore City from 1999 to 2008")
            )
     )
dev.off()


