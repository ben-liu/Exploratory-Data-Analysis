# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Course Project 2")

NEI <- readRDS("./exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("./exdata-data-NEI_data/Source_Classification_Code.rds")

# 5.  How have emissions from motor vehicle sources changed from 1999–2008 
# in Baltimore City?

mv.code= SCC[grep(".*Vehicle|.*Motor",SCC$EI.Sector),]
mv=NEI[NEI$SCC %in% mv.code$SCC & NEI$fips %in% "24510",]
x = with(mv,aggregate(Emissions,by=list(year),sum))
names(x) = c("Year","Emissions")
with(x,plot(Year,Emissions,type="l",
            ylab = expression('Total PM'[2.5]*" Emission"),
            main="Total PM2.5 emission from motor vehicle \n from 1999 to 2008 in Baltimore City")
            )
    )

dev.copy(png,file="plot5.png")
dev.off()


