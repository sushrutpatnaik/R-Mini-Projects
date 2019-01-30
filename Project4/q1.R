#set Working Directory
setwd("C:/Project4")


library(boot)

#read the file for GPA and ACT scores
gpa=read.csv(file="gpa.csv", header = TRUE, sep=',')
plot(gpa$gpa,gpa$act, xlab="GPA", ylab = "ACT", pch=20)



#find the population correlation(p) between GPA and ACT
p=cor(gpa$gpa,gpa$act) 
print(paste("Point Estimate of correlation between GPA and ACT Scores is:", p))

set.seed(1234)
#function to find correlation between GPA and ACT 
correlation.npar=function(x, indices)
{
  result=cor(x[indices,1], x[indices,2])
  return(result)
}
set.seed(1234)

#find the point estimate, bias and standard error  
correlation.npar.boot = boot(gpa, correlation.npar, R = 999, sim = "ordinary", stype = "i")
correlation.npar.boot

#95% confidence interval using percentile bootstrap
boot.ci(correlation.npar.boot,conf=0.95,type="perc") or
sort(correlation.npar.boot$t)[c(25, 975)]
