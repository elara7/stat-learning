# Example (June 2016) of using taining and test sets to assess accuracy.
# WARNING: Although I have posted this, it has advanced R commands in
# it and is not directly relevant for the assignemnts.  The curious student is
# welcome to try it out.  Beginners will find it daunting unless they are 
# comfortable with other programming languages.

pdf('02testexample.pdf')
# NOTE - the pdf command redirects all plots to a pdf file.  
# I have commented it out here because students trying the code will 
# want to see their plot in R.
set.seed(105)
sigma = .5  # the standard deviation of the error term
# Define the "true" function f(x)
f = function(x){
  sin(2*pi*x^2)*exp(x)
}

n = 20 # size of training set
x=runif(n)  # sample n training points from a U(0,1) distribution
y = f(x)+rnorm(n,0,sigma) # simulate Y values with normal errors
x.grid=seq(0,1,l=500)  # fine grid of X values for plotting smooth curve.
plot(x,y,main='training data with true function plotted',ylim=range(c(y,f(x.grid))))
lines(x.grid,f(x.grid))

# Below I define a function that generates one fitted models for a polynomial
# of maximum degree "degree".  It also calculates and records the training
# and test set errors.  This function is used after it is defined (see below)

do.test = function(degree,x,y,f,sigma,plotit=TRUE,...){
  mylm = lm(y~poly(x,degree),data=data.frame(y=y,x=x))
  yhat.train = predict(mylm)
  train.mse = mean((yhat.train-y)^2)
  if (plotit) {
    plot(x,y,main=paste('training data, degree=',degree),...)
    x.grid = seq(0,1,l=500)
    lines(x.grid,predict(mylm,newdata=data.frame(x=x.grid)))
  }
  n.test = length(x)+5  # test set need not be same size as training
  x.test = seq(0,1,length=n.test)  
  # test set above is a grid, but using runif() would have been OK also.  
  y.test = f(x.test)+rnorm(n.test,0,sigma)
  if (plotit) {
    plot(x.test,y.test,pch=19,
         main=paste('test data, degree=',degree),...)
    lines(x.grid,predict(mylm,newdata=data.frame(x=x.grid)))
  }
  yhat.test = predict(mylm,newdata=data.frame(x=x.test))
  if (plotit) 
    for (i in 1:n.test)
      lines(rep(x.test[i],2),c(y.test[i],yhat.test[i]),col='green')
  test.mse = mean((yhat.test-y.test)^2)
  return(c(train.mse,test.mse))
}

par(mfrow=c(1,2)) # Set graphics window so we get 2 plots on one "page"
do.test(2,x,y,f,sigma)
# above tries out one simulation, with polynomial of max degree 2.  You can re-run
# it with different "degree" values.

# code below runs 10 iterations, for polynomials of max degree 1, 2, ..., 10
par(mfrow=c(1,2))
maxdegree = 10
test.mse = train.mse = rep(0,maxdegree)
for (i in 1:maxdegree) {
  tmp = do.test(i,x,y,f,sigma,plotit=TRUE,ylim=c(-3,3),xlim=c(0,1))
  train.mse[i] = tmp[1]
  test.mse[i] = tmp[2]
  readline(prompt="hit enter to continue")
  # If you remove or comment the readline command, the loop will execute without pause
}

# Generate a plot of training and test MSE vs. degree of polynomial.
par(mfrow=c(1,1))
matplot(1:maxdegree,cbind(train.mse,test.mse),xlab='degree',type='l',ylim=c(0,4),
        ylab='MSE')
legend('topleft',lty=1:2,legend=c('train','test'))
#dev.off()
# NOTE: If you use pdf(), the way to stop saving output to a file is to use dev.off()


# Another Example:
# dose response classification example
x = runif(100,0,10)
f = function(x){
  p = exp((x-5)*1.5)
  p/(1+p)
}
y = rbinom(100,1,f(x))
plot(x,y)
xgrid = seq(0,10,l=500)
lines(xgrid,f(xgrid))
abline(v=5.5,col='grey',lty=2)
abline(v=c(5,6),col='grey',lty=3)
