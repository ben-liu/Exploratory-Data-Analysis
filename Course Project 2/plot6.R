# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 2")

NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")

# 6.  Compare emissions from motor vehicle sources in Baltimore City
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