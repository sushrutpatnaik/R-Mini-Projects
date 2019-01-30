setwd("C:/RData")

data <- read.csv('motorcycle.csv', header = T)
count <- data[[2]]

boxplot()
