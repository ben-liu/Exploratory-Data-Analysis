# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 2")

NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")

# 3.  Of the four types of sources indicated by the type 
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