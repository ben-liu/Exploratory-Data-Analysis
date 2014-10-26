
# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 2")
# setwd("C:/Users/Ben/Google Drive/Coursera/Exploratory Data Analysis/Course Project 2")
# setwd("C:/Users/Harry Samsung Laptop/Downloads/ben.liu's work/Coursera/Exploratory Data Analysis/Course Project 2")


NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")


source("load.r")

# 1.  Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? Using the base plotting system, 
# make a plot showing the total PM2.5 emission from all sources for 
# each of the years 1999, 2002, 2005, and 2008.

x = with(NEI,aggregate(Emissions,by=list(year),sum))
names(x) = c("Year","Emissions")
with(x,plot(Year,Emissions,type="l"))
title(main="Total PM2.5 emission from 1999 to 2008")
dev.copy(png,file="plot1.png")
dev.off()


# 2.	Have total emissions from PM2.5 decreased in the 
# Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
# Use the baseplotting system to make a plot answering this question.

bal= NEI[NEI$fips %in% "24510",]
x = with(bal,aggregate(Emissions,by=list(year),sum))
names(x) = c("Year","Emissions")
with(x,plot(Year,Emissions,type="l"))
title(main="Total PM2.5 emission in \n Baltimore City from 1999 to 2008")
dev.copy(png,file="plot2.png")
dev.off()



# 3.	Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, which of these four sources 
# have seen decreases in emissions from 1999–2008 for Baltimore City? 
# Which have seen increases in emissions from 1999–2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

library(ggplot2)

bal= NEI[NEI$fips %in% "24510",]

ggplot(bal, aes(year, Emissions, color = type)) + 
  stat_summary(geom="line", fun.y="sum")+ 
  xlab("Year") +
  ylab(expression('Total PM'[2.5]*" Emissions")) + 
  ggtitle("Total Emissions in Baltimore City from 1999 to 2008")

dev.copy(png,file="plot3.png")
dev.off()

# 4.	Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

cc.code= SCC[grep(".*Comb.*Coal",SCC$EI.Sector),]
cc=NEI[NEI$SCC %in% cc.code$SCC,]

x = with(cc,aggregate(Emissions,by=list(year),sum))
names(x) = c("Year","Emissions")
with(x,plot(Year,Emissions,type="l"))
title(main="Total PM2.5 emission from coal \n combustion-related from 1999 to 2008")
dev.copy(png,file="plot4.png")
dev.off()

# 5.	How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

mv.code= SCC[grep(".*Vehicle|.*Motor",SCC$EI.Sector),]
mv=NEI[NEI$SCC %in% mv.code$SCC & NEI$fips %in% "24510",]
x = with(mv,aggregate(Emissions,by=list(year),sum))
names(x) = c("Year","Emissions")
with(x,plot(Year,Emissions,type="l"))
title(main="Total PM2.5 emission from motor vehicle \n from 1999 to 2008 in Baltimore City")
dev.copy(png,file="plot5.png")
dev.off()



motor <- ddply(NEI[NEI$fips == "24510" 
                    & NEI$type == "ON-ROAD",],
                .(type,year), summarise, 
                TotalEmissions = sum(Emissions))


# 6.	Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037"). Which city has seen greater changes 
# over time in motor vehicle emissions?


mv.code= SCC[grep(".*Vehicle|.*Motor",SCC$EI.Sector),]
mv=NEI[NEI$SCC %in% mv.code$SCC & NEI$fips %in% c("24510","06037"),]

ggplot(mv, aes(year, Emissions, color = fips)) + 
  stat_summary(geom="line", fun.y="sum")+ 
  scale_colour_discrete(name = "Baltimore vs LA", breaks =c("24510","06037") ,labels = c("Baltimore","Los Angeles")) + 
  ylab(expression('Total PM'[2.5]*" Emissions")) + 
  ggtitle("Total PM2.5 Emissions From Motor Vehicle \n Sources in Baltimore City vs Los Angeles County \n from 1999 to 2008")

dev.copy(png,file="plot6.png")
dev.off()

