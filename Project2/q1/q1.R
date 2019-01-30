setwd("C:/RData")

#Q1.a) 
data <- read.csv('roadrace.csv', header = T)
count <- table(data$Maine)
barplot(count,space=0.1, col='gold')

#b)


data_away <-subset(data, Maine == "Away")
data_maine <-subset(data, Maine == "Maine")

away_running <- data_away[[12]]
maine_running <- data_maine[[12]]

par(mfrow=c(1,2))
hist(away_running, main = "Runners from Maine",xlim=c(20,120),ylim=c(0,2000), xlab = "Running Time(in mins)")
hist(maine_running, main = "Runners from Other Places",xlim=c(20,120),ylim=c(0,2000), xlab = "Running Time(in mins)")




#c)
par(mfrow=c(1,1))
boxplot(away_running,maine_running, ylim=c(20,140),ylab="Running times", names=c("Away runners","Maine runners"), main="Boxplots of Running times")


#d)
male_count <- subset(data, Sex == "M")
female_count <- subset(data, Sex == "F")

male_age <- as.numeric(male_count[[5]])
female_age <- as.numeric(female_count[[5]])

boxplot(male_age,female_age, ylim=c(0,80),ylab="Ages", names=c("Male Ages","Female Agess"), main="Boxplots of Ages of Females and Males")




