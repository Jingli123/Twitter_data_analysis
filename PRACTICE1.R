##Problem1
## load the data file 
hw1 <- read.csv("/Users/jingli/Desktop/M01_quasi_twitter.csv")
View(hw1)
## Load the data from the file "M01_quasi_twitter.csv"
hw1 <- read.csv("/Users/yuhe/Desktop/M01_quasi_twitter.csv")
View(hw1)

## Create the data distribution of friends_count
hist(hw1$friends_count)
install.packages("logging")
hist(log(hw1$friends_count), xlab = "log friends_count", col = "red", main = "Distribution of log friends_count")

##Compute the summery statistics on friends_count?
summary(hw1$friends_count)

##evaluate the data quality of friends_count
##whether the data has NULL
sum(is.na(hw1$friends_count))

##whether the data has minus valaues
sum(hw1$friendsg_count < 0)
##count the data number of friends_count
sum(hw1$friends_count)

## d. Produce a 3D scatter plot with highlighting
install.packages("scatterplot3d")
library(scatterplot3d) 
x <- hw1$created_at_year
y <- hw1$education
z <- hw1$age
scatterplot3d(x, y, z, highlight.3d = TRUE, main = "3D scatter plot")

## e. pie chart with percentages
library(plotrix)
par(mfrow=c(1,2))
twitter_account <- c(650,1000,900,300,14900)
twitter_country <- c("UK","Canada","India","Australia","US")
percent <- round(twitter_account/sum(twitter_account)*100)
twitter_country <- paste(twitter_country, percent)# add percents to labels
twitter_country <- paste(twitter_country, "%", sep ="")# ad % to labels 
pie(twitter_account,labels = twitter_country,main = "Pie Chrt for the Country")

##3D pie chart
install.packages("plotrix")
library(plotrix)
pie3D(twitter_account,labels = twitter_country, explode = 0.1,main = "Pie Chart of Countries")

## f. kernel density plot
par(mfrow=c(1,1))
kernel_density <- density(hw1$created_at_year)
plot(kernel_density)
## in 2006, there is a few twitter user.Since 2008,the user of twitter start to increase. It reaches the maximum number,around 0.28, in 2009.The curve of density of created user is wavy from 2010 to 2015. The tendency is decreasing from 2015 to 2016

##Problem2
##load data cereals.csv
cereals<- read.csv("/Users/jingli/Desktop/Cereals.csv")
View(cereals)

sapply(cereals[,4:16],mean,na.rm=TRUE)
sapply(cereals[,4:16],median,na.rm=TRUE)
sapply(cereals[,4:16],max,na.rm=TRUE)
sapply(cereals[,4:16],sd,na.rm=TRUE)

par(mfrow=c(3,4))
hist(cereals$calories)
hist(cereals$protein)
hist(cereals$fat)
hist(cereals$sodium)
hist(cereals$fiber)
hist(cereals$carbo)
hist(cereals$sugars)
hist(cereals$potass)
hist(cereals$vitamins)
hist(cereals$weight)
hist(cereals$cups)
hist(cereals$rating)
#boxplot(cereals[,6])
#for (col in 4:12){
#  hist(cereals[,col])
#}

boxplot(cereals$calories~cereals$type)
boxplot(cereals$rating~cereals$shelf)
cor(cereals[4:16])
plot(cereals[4:16])
pcs.cor <- prcomp(na.omit(cereals[,-c(1:3)]))
summary(pcs.cor)

##Problem3
bostonhousing<-read.csv("/Users/jingli/Desktop/BostonHousing.csv")
View(bostonhousing)
min <- sapply(bostonhousing,function(x) min(x))
max<-sapply(bostonhousing,function(x) max(x))
max_min <- bostonhousing
for(i in 1:14){max_min[,i]= (max_min[,i]-min[i])/(max[i]-min[i])}

bostonhousing<-read.csv("/Users/jingli/Desktop/BostonHousing.csv")
cor(bostonhousing)[,14]
order(cor(bostonhousing)[,14],decreasing = TRUE)

bostonhousing<-read.csv("/Users/jingli/Desktop/BostonHousing.csv")
pca_bh<-prcomp(bostonhousing)
summary(pca_bh)
pca_bhs<-prcomp(max_min)
summary(pca_bhs)

intall.packages("devtools")
install.packages("ggplot2")
library(devtools)
install_github("ggbiplot", "vqv")
## Warning: Username parameter is deprecated. Please use vqv/ggbiplot
## Skipping install of 'ggbiplot' from a github remote, the SHA1 (7325e880) has not changed since last install.
##   Use `force = TRUE` to force installation
library(ggbiplot)
## Loading required package: ggplot2
## 
## Attaching package: 'ggplot2'
## The following objects are masked from 'package:psych':
## 
##     %+%, alpha
## Loading required package: plyr
## Loading required package: scales
## 
## Attaching package: 'scales'
## The following objects are masked from 'package:psych':
## 
##     alpha, rescale
## The following object is masked from 'package:plotrix':
## 
##     rescale
## Loading required package: grid
pca_bh<-prcomp(bostonhousing)
g1 <- ggbiplot(pca_bh, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
g1 <- g1 + scale_color_discrete(name = '')
g1 <- g1 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g1)

min <- sapply(bostonhousing,function(x) min(x))
max<-sapply(bostonhousing,function(x) max(x))
max_min <- bostonhousing
for(i in 1:14){max_min[,i]= (max_min[,i]-min[i])/(max[i]-min[i])}
pca_bhs<-prcomp(max_min)

g2 <- ggbiplot(pca_bhs, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
g2 <- g2 + scale_color_discrete(name = '')
g2 <- g2 + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g2)



##problem4
wine <-read.csv("/Users/jingli/Desktop/Wine.csv")
View(wine)
pcs.cor <- prcomp(wine[,-1])
summary(pcs.cor)


