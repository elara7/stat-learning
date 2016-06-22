# R Commands used in Ch 3

#
# Simple linear regression
#
# The "Advertising" data is not contained in the ISLR package, but can be 
# obtained from your instructor
Advertising = read.csv("C:/jjiang/courses/STAT6115/notes/statistical learning/statistical learning - Summer2016-Xiamen Univ/Rcodes/Advertising.csv", row.names=1)
# some numeric and graphical summaries
summary(Advertising)

# Fit the simple linear regression model with predictor TV and response Sales
mylm = lm(Sales~TV,data=Advertising)
plot(Advertising$TV,Advertising$Sales)
abline(mylm) 
# abline draws a line, when given an intercept and slope, or a linear model
summary(mylm) # displays basic summaries of the lm
# Note that the basic summaries above include much of the information That we 
# need, including coefficient estimates, standard errors, t-stat and p-values 
# for testing a null hypothesis of a zero value, and residual standard error,
# R^2 and a F-statistic (see later).

confint(mylm)  # makes confidence intervals for the coefficients of the lm

# show some diagnostics for the fitted model
par(mfrow=c(2,2))
plot(mylm)  

#
# Multiple linear regression
#

pairs(Advertising[,1:3])  # look at the X variables
cor(Advertising[,1:3])

mylm = lm(Sales~TV+Radio+Newspaper,data=Advertising)
summary(mylm)

# above summary gives us a lot of information including "important question 1"
# about whether any of the coefficients are nonzero.

# Stepwise variable selection
step(mylm)  # default is backward when just a model is given.

# to do forward stepwise we have to first fit a "null" model with just an
# intercept
nulllm = lm(Sales~1,data=Advertising)
mylm2 = step(nulllm,scope=list(lower=nulllm,upper=mylm),direction='forward')
# the above command is different in several ways from forward stepwise.  1) it
# specifies a lower and upper bound of models to consider, and 2) the result is
# assigned to "mylm2", saving the resultant linear model as a new model.
summary(mylm2)
# we can manipulate "mylm2" just like a model we fit via "lm" (above)

# confidence and prediction intervals at specific inputs.
predict(mylm2,newdata=data.frame(TV=100,Radio=20),interval='confidence')
predict(mylm2,newdata=data.frame(TV=100,Radio=20),interval='prediction')

# the "credit" data is used as another example, in the context of a qualitative
# predictors
credit = read.csv("C:/jjiang/courses/STAT6115/notes/statistical learning/statistical learning - Summer2016-Xiamen Univ/Rcodes/Credit.csv",
                  row.names=1)
summary(lm(Balance~Gender,data=credit))
summary(lm(Balance~Ethnicity,data=credit))
summary(lm(Balance~Gender+Ethnicity+Income,data=credit))
summary(lm(Balance~Income*Gender,data=credit))
# we didn't do the last two.  They are 1) a model with indicator variables for 
# two different qualitative variables, and 2) an iteraction between income and
# gender. In just these models it seems like only Income is significant among
# the variables I considered.  I did not do a full stepwise search, you could
# try this.

# Interactions in the advertising model:
summary(lm(Sales~TV+Radio+TV:Radio,data=Advertising))

# polynomial fitting, illustrated with the Auto data
library(ISLR)
x.grid = seq(min(Auto$horsepower),max(Auto$horsepower),l=500)
lm1 = lm(mpg~horsepower,data=Auto)
yhat1 = predict(lm1,newdata=data.frame(horsepower=x.grid))
plot(Auto$horsepower,Auto$mpg)
lines(x.grid,yhat1)
# Do the same as above for a quadratic
lm2 = lm(mpg~poly(horsepower,2),data=Auto)
yhat2 = predict(lm2,newdata=data.frame(horsepower=x.grid))
lines(x.grid,yhat2)

# diagnostic plots for the Advertising data, assuming you've already loaded the
# data as above.
lm1 = lm(Sales~TV+Radio+Radio:TV,data=Advertising)
par(mfrow=c(2,2)) # prepare for the 4 diagnostic plots
plot(lm1) # Plot diagnostics

lm2 = lm(Sales~poly(TV,2)+Radio+Radio:TV,data=Advertising)
summary(lm2)
plot(lm2)
# points 131 and 156 seem to be outliers
mycol = rep('green',nrow(Advertising))
mycol[c(131,156)]='red'
pairs(Advertising,col=mycol)
# so they're points with exceptionall low sales, low TV & newspaper spending, 
# and one Radio spend is low, the other high.  The corresponding sales are
# really low.
Advertising[c(131,156),]
#install.packages("ISLR")
#library(ISLR)
