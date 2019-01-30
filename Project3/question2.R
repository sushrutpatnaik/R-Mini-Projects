input<-c(21.72,14.65,50.42,28.78,11.23)


#define the function given by the pdf in question
mletheta<-function(x,a)
{ 
  return(a/x^(a+1))
}

# Negative of log-likelihood function
neg.loglik.fun<-function(dat,par){
  result<-sum(log(mletheta(dat,par)))
  return(-result)}

# Minimize -log (L), i.e., maximize log (L)
ml.est<-optim(par=1, fn=neg.loglik.fun, method ="BFGS",hessian=TRUE, dat=input)

#find the parameter estimate
ml.est$par

#Standard error of the estimate
sqrt(diag(solve(ml.est$hessian)))

#95% confidence Interval of the estimate
upper<-ml.est$par + 1.96 * sqrt(diag(solve(ml.est$hessian)))
lower<-ml.est$par - 1.96 * sqrt(diag(solve(ml.est$hessian)))

interval<-data.frame(upper=upper, lower=lower)
interval




