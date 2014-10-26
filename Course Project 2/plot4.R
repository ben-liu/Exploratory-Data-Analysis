# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 2")

NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")

# 4.  Across the United States, how have emissions from coal combustion-related
# sources changed from 1999 – 2008 ?

cc.code= SCC[grep(".*Comb.*Coal",SCC$EI.Sector),]
cc=NEI[NEI$SCC %in% cc.code$SCC,]

x = with(cc,aggregate(Emissions,by=list(year),sum))
names(x) = c("Year","Emissions")

png(file="plot4.png")
par(mar=c(5,5,4,2)+0.1)
with(x,plot(Year,Emissions,type="l",
            ylab = expression('Total PM'[2.5]*" Emission"),
            main="Total PM2.5 emission from coal \n combustion-related from 1999 to 2008"
            )
     )
dev.off()



