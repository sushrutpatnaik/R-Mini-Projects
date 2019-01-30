


# function to find MSE
mse<-function(n, theta) 
{
  
  #function to find both MLE and MOME
  
  computestimate<-function(n, theta) 
  {
    x<-runif(n, min = 0, max = theta)
    mle<-max(x)
    mome<-2*mean(x)
    return(c(mle=mle,mome=mome))
  }
  
  # replicate 1000 times
  estimate<-replicate(1000, computestimate(n, theta))
  
  # return MSE value(MLE and MOME) for a particular n and theta
  return(rowMeans((estimate - theta)^2))
}


#########################################################

n = c(1, 2, 3, 5, 10, 30)
theta = c(1, 5, 50, 100)
set.seed(1)

#get length of n and theta matrix
nlength=length(n)
thetalength=length(theta)

#create two (n x theta) matrices to store MSE of MLE and MOME for every combination of n and theta
mseofmle<-matrix(nrow = nlength, ncol = thetalength)
mseofmome<-matrix(nrow = nlength, ncol = thetalength)


for(i in 1:nlength) 
{
  for(j in 1:thetalength) 
  {
    #call mse function for n=n[i] and theta=theta[j] 
    result<-mse(n[i],theta[j])
    mseofmle[i,j]<-result["mle"]
    mseofmome[i,j]<-result["mome"]
  } 
}

#dislay the MSE matrices for MLE and MOME for all Combinations of n, theta
mseofmle
mseofmome


#plotting the MSE graphs(MLE and MOME)on the same plot for each value of theta

par(mfrow=c(2, 2))
for(i in 1:thetalength) {
  #MLE plot in solid line
  plot(n, mseofmle[,i], lty="solid",type="l", main=paste("theta",theta[i]), ylab="MSE", ylim=c(0, max(mseofmome[, i], mseofmle[, i])))
  #MOME plot in dotted lines
  lines(n, mseofmome[,i],lty="dotted")
}

conf


