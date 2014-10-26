# setwd("~/Google Drive/Coursera/Exploratory Data Analysis/Week 1 Quiz")


pollution<- read.csv("data/avgpm25.csv",colClasses=c("numeric","character","factor","numeric","numeric"))

# 5/6 figure summary
summary(pollution$pm25)

# boxplot
boxplot(pollution$pm25,col="blue")
abline(h=12) #horizontal line at 12


# histogram
hist(pollution$pm25,col="green")
rug(pollution$pm25) # plot all points below

hist(pollution$pm25,col="green",breaks=100)

abline(v=12,lwd=2) # vertical, line width
abline(v=median(pollution$pm25),col="magenta",lwd=4)

# barplot (cope with categorical factors)
barplot(table(pollution$pm25),col="wheat",main="number of counties in each region")


boxplot(pm25 ~region,data=pollution,col="red")

with(pollution,plot(latitude,pm25))
abline(h=12,lwd=2,lty=2)


# multiple scatterplots
par(mfrow=c(1,2),mar=c(5,4,2,1))
with(subset(pollution,region=="west"),plot(latitude,pm25,main="west"))
with(subset(pollution,region=="east"),plot(latitude,pm25,main="east"))




# different plotting system
# basis plotting
# use annotation functions to add/modify (eg text,lines,points,axis)

library(dataset)
data(cars)
with(cars,plot(speed,dist))

# lattice system
xyplot()
bwplot()


library(lattice)
state<- data.frame(state.x77,region=state.region)
xyplot(Life.Exp ~Income | region, data=state,layout=c(4,1))

# ggplot2
library(ggplot2)
data(mpg)
qplot(displ,hwy,data=mpg)