setwd("C:/Project4")
#read the file for GPA and ACT scores
data=read.csv(file="VOLTAGE.csv", header = TRUE, sep=',')




#separate the data for remote and local locations
remote=subset(data, location=="0")
local=subset(data, location=="1")

#summary Statistics of the Remote Locations
summary(remote$voltage)
IQR(remote$voltage)
sd(remote$voltage)

#summary Statistics of the local Locations
summary(local$voltage)
IQR(local$voltage)
sd(local$voltage)

#Boxplots of Voltage readings at local and remote
par(mfrow = c(1, 1))
boxplot(remote$voltage,local$voltage,ylab="Voltage Readings",names=c("Remote","Local"), main="Boxplots of Voltage readings")

#t-test to find the 95% CI
t.test(remote$voltage, local$voltage, alternative = "two.sided", conf.level = 0.95, var.equal = FALSE)


#normal qqPlots
par(mfrow=c(1,2))
qqnorm(remote$voltage, main = "Remote")
qqnorm(local$voltage, main = "Local")




