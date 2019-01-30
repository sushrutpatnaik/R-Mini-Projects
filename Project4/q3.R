setwd("C:/Project4")
#read the file for theoritical and experimental values of the vapor pressure
data=read.csv(file="VAPOR.csv", header = TRUE, sep=',')


# boxplots
par(mfrow = c(1, 1))
boxplot(data$theoretical,data$experimental,names=c("Theoritical","Experimental"))


# histograms and QQ plots for theoritical and experimental values
par(mfrow = c(1, 2))
hist(data[,2],main="Histogram of Theoritical values", xlab = "Theoritical values")
qqnorm(data[,2])
qqline(data[,2])

hist(data[,3],main="Histogram of Experimental values", xlab = "Experimental values")
qqnorm(data[,3])
qqline(data[,3])


# histogram and QQ plot of differences

diff <- data[,2] - data[,3]

boxplot(diff, main)
hist(diff)
qqnorm(diff)
qqline(diff)



set.seed(1234)
#function to calculate mean
calculated.mean <- function(x, indices) {
  result= mean(x[indices])
  return (result)
}

mean.boot=boot(diff,calculated.mean, R = 999, sim = "ordinary", stype = "i")
boot.ci(mean.boot,conf=0.95,type="perc") 






