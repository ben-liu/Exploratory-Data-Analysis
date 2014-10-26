# Lattice Plotlibrary(lattice)


library(datasets)
## Simple scatterplot
xyplot(Ozone ~ Wind, data = airquality)



library(datasets)
library(lattice)
## Convert 'Month' to a factor variable
airquality <- transform(airquality, Month = factor(Month)) 
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5, 1))



# Lattice Behavior

p <- xyplot(Ozone ~ Wind, data = airquality)  ## Nothing happens!
print(p)  ## Plot appears

xyplot(Ozone ~ Wind, data = airquality)  ## Auto-printing


# Lattice Panel Functions
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
y <- x + f - f * x+ rnorm(100, sd = 0.5)
f <- factor(f, labels = c("Group 1", "Group 2"))
xyplot(y ~ x | f, layout = c(2, 1))  ## Plot with 2 panels


## Custom panel function - add line of mean 
xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)  ## First call the default panel function for 'xyplot'
  panel.abline(h = median(y), lty = 2)  ## Add a horizontal line at the median
})

## Custom panel function 
xyplot(y ~ x | f, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)  ## First call default panel function
  panel.lmline(x, y, col = 2)  ## Overlay a simple linear regression line
})


# cannot mix functions between different plotting systems


# Many Panel Lattice Plotlibrary(lattice)
env <- readRDS("maacs_env.rds")
env <- transform(env, MxNum = factor(MxNum))
xyplot(log2(airmus) ~ VisitNum | MxNum, data = env, strip = FALSE, pch = 20, xlab = "Visit Number", ylab = expression(Log[2] * " Airborne Mouse Allergen"), main = "Mouse Allergen and Asthma Cohort Study (Baltimore City)")


# ggplot2

# Plots are made up of aesthetics (size, shape, color) and geoms (points, lines)

# Example Dataset

library(ggplot2)
str(mpg)
qplot(displ, hwy, data = mpg)

# Modifying aesthetics
qplot(displ, hwy, data = mpg, color = drv)

# Adding a geom
qplot(displ, hwy, data = mpg, geom = c("point", "smooth"))

# Histograms
qplot(hwy, data = mpg, fill = drv)

# Facets
qplot(displ, hwy, data = mpg, facets = . ~ drv)
qplot(hwy, data = mpg, facets = drv ~ ., binwidth = 2)


# MAACS Cohort
eno <- read.csv("eno.csv") # exhaled nitric oxide
skin <- read.csv("skin.csv")
env <- read.csv("environmental.csv")
m <- merge(eno, env, by = "id")
maacs <- merge(m, skin, by = "id")


# Histogram of eNO
qplot(log(eno), data = maacs)

# Histogram by Group
qplot(log(eno), data = maacs, fill = mopos)

# Density Smooth
qplot(log(eno), data = maacs, geom = "density")
qplot(log(eno), data = maacs, geom = "density", color = mopos)

# Hierarchical clustering - example
set.seed(1234); par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

# Important parameters: x,method
dataFrame <- data.frame(x=x,y=y)
dist(dataFrame)

# Hierarchical clustering - #1
suppressMessages(library(fields))
dataFrame <- data.frame(x=x,y=y)
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

# Find the index of the points with minimum distance
ind <- which(rdistxy == min(rdistxy),arr.ind=TRUE)
par(mfrow=c(1,2),mar=rep(0.2,4))
# Plot the points with the minimum overlayed
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
points(x[ind[1,]],y[ind[1,]],col="orange",pch=19,cex=2)

# Make a cluster and cut it at the right height
distxy <- dist(dataFrame)
hcluster <- hclust(distxy)
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro,h=(hcluster$height[1]+0.00001) )
plot(cutDendro$lower[[11]],yaxt="n")


# Hierarchical clustering - #2
  
library(fields)
dataFrame <- data.frame(x=x,y=y)
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

# Find the index of the points with minimum distance
ind <- which(rdistxy == min(rdistxy),arr.ind=TRUE)
par(mar=rep(0.2,4))
# Plot the points with the minimum overlayed
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
points(x[ind[1,]],y[ind[1,]],col="orange",pch=19,cex=2)
points(mean(x[ind[1,]]),mean(y[ind[1,]]),col="black",cex=3,lwd=3,pch=3)
points(mean(x[ind[1,]]),mean(y[ind[1,]]),col="orange",cex=5,lwd=3,pch=1)


# Hierarchical clustering - #3
  
library(fields)
dataFrame <- data.frame(x=x,y=y)
rdistxy <- rdist(dataFrame)
diag(rdistxy) <- diag(rdistxy) + 1e5

# Find the index of the points with minimum distance
ind <- which(rdistxy == rdistxy[order(rdistxy)][3],arr.ind=TRUE)
par(mfrow=c(1,3),mar=rep(0.2,4))
# Plot the points with the minimum overlayed
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))
points(x[c(5,6)],y[c(5,6)],col="orange",pch=19,cex=2)
points(x[ind[1,]],y[ind[1,]],col="red",pch=19,cex=2)

# Make dendogram plots
distxy <- dist(dataFrame)
hcluster <- hclust(distxy)
dendro <- as.dendrogram(hcluster)
cutDendro <- cut(dendro,h=(hcluster$height[2]) )
plot(cutDendro$lower[[10]],yaxt="n")
plot(cutDendro$lower[[5]],yaxt="n")


# Prettier dendrograms

myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
  ## modifiction of plclust for plotting hclust objects *in colour*!
  ## Copyright Eva KF Chan 2009
  ## Arguments:
  ##    hclust:    hclust object
  ##    lab:        a character vector of labels of the leaves of the tree
  ##    lab.col:    colour for the labels; NA=default device foreground colour
  ##    hang:     as in hclust & plclust
  ## Side effect:
  ##    A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
  y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
  y <- y[order(x)]; x <- x[order(x)]
  plot( hclust, labels=FALSE, hang=hang, ... )
  text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
        labels=lab[hclust$order], col=lab.col[hclust$order], 
        srt=90, adj=c(1,0.5), xpd=NA, ... )
}

dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))


# Merging points - complete
dataFrame <- data.frame(x=x,y=y)
par(mar=rep(0.1,4))
plot(x,y,col="blue",pch=19,cex=2)
points(x[8],y[8],col="orange",pch=3,lwd=3,cex=3)
points(x[1],y[1],col="orange",pch=3,lwd=3,cex=3)
segments(x[8],y[8],x[1],y[1],lwd=3,col="orange")


# Merging points - average
dataFrame <- data.frame(x=x,y=y)
par(mar=rep(0.1,4))
plot(x,y,col="blue",pch=19,cex=2)
points(mean(x[1:4]),mean(y[1:4]),col="orange",pch=3,lwd=3,cex=3)
points(mean(x[5:8]),mean(y[5:8]),col="orange",pch=3,lwd=3,cex=3)
segments(mean(x[1:4]),mean(y[1:4]),mean(x[5:8]),mean(y[5:8]),lwd=3,col="orange")

# heatmap()
dataFrame <- data.frame(x=x,y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix)
