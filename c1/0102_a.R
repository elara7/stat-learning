
library(ISLR)
library(splines)

data(Wage)
dim(Wage)
names(Wage)
attach(Wage)
W1=summary(Wage)

agelims=range(age)
age.grid=seq(from=agelims[1], to=agelims[2])

# use the dataset to draw the graph 
# Fit a natural spline with four degrees of freedom with function "ns" #
fit2 = lm(wage~ns(age, df=4), data=Wage)
pred=predict(fit2, newdata=list(age=age.grid), se=T)

par(mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))

plot(age,wage,col="gray")
title("Natural Cubic Splines", outer=T)
lines(age.grid,pred$fit,lwd=2, col="red")
lines(age.grid,pred$fit+2*pred$se, lty="dashed")
lines(age.grid,pred$fit-2*pred$se, lty="dashed")

