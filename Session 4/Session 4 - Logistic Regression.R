##Case: Credit Card default data
#We'd like to predict customers that are likely to default
#
#Possible X variables:
# - Annual Income
# - Monthly credit card balance:
#   - 0 (nothing owed)
#   - positive (something owed)
#   - negative (payment made over what is owed)
#
#Y variable (default) is categorical: Yes or No
#
#How do we check the relationship between:Y and X?
#
#Load the libaries
library(MASS)
library(ISLR)
#view data set
fix(Default)

#first trial of plotting the relationship between balance and income
plot(Default$balance,Default$income)
#adding colours and legends. Blue circles are for not default and orange + are for default.
plot(Default$balance,Default$income,col=ifelse(Default$default=='No',"blue","orange"),pch=ifelse(Default$default=='No',"o","+"))

#plot the relationship between default and balance with colour
colours=c("blue","orange")
plot(Default$default,Default$balance,xlab="Default",ylab="Balance",col=colours)

#plot the relationship between balance and default, but the values are 1s and 2s.
plot(Default$balance,Default$default,col="orange")
##the following code turns 1s and 2s into 0s and 1s...
#as.numeric turns a qualitative variable to a quantitative variable.
#however, R will automatically assign 1 to "No" and 2 to "Yes"
as.numeric(Default$default)
#...so we subtract 1 from all the default values and make it a new vector default_num
default_num=as.numeric(Default$default)-1
#and now the plot shows 0s and 1s!...
plot(Default$balance,default_num,col="orange")

#Next we will build the linear regression model, the plot it (not sure why...)
lm(default_num~balance,data = Default)
#look at results from above...
fit_linear=lm(default_num~balance,data = Default)
#add the line into the plot
abline(fit_linear,col="blue")

#Next we build the logistic regression model
glm.fit=glm(default_num~balance,data = Default,family=binomial)
summary(glm.fit)

##We will now plot the result of the logistic regression. 3 ways to do this...

#1. First way is sort of cheating, as instead of a smooth curve we plot the dots
plot(Default$balance,glm.fit$fit,col="blue",pch=".")

#2. Using the inverse function of logit
plot(Default$balance,default_num,col="orange")
#we take sample numbers from balance
xrange=seq(min(Default$balance),max(Default$balance),length.out=100)
#not sure why we need this
library(boot)
lines(xrange,inv.logit(glm.fit$coef[1]+glm.fit$coef[2]*xrange),col="red")

#3. Making prediction first and plot the predicted value as a smooth line - PREFERRED WAY
y=Default$default
x=Default$balance
glm.fit_1=glm(y~x,family=binomial)
#making predictions
yrange=predict(glm.fit_1,data.frame(x=xrange),type="response")
lines(xrange,yrange,col="red")

##How to predict using the logisitic model
#We predict the probability of default when an individual has an average balance of 1000 and 2000
glm.fit=glm(default~balance,data = Default,family=binomial)
newy=predict(glm.fit,data.frame(balance=c(1000,2000)),type="response")
newy
#above shows prob for default for a person with balance $1000 is less than 1%
#for a balance of $2000, prob is much higher (58.6%)

##The following code builds a logistic regression model between two qualitative variables
glm.fit_student=glm(default~student,data=Default,family=binomial)
summary(glm.fit_student)
#above shows ^B1 is positive: indicates students higher default probabilities
#The following predicts the prob of default given student or not student:
#Two ways of doing this:

#1. Use the student model directly (student model is built from two qualitative variables)
newy=predict(glm.fit_student,data.frame(student=c("No","Yes")),type="response")
newy

#2. Use make student as a numeric value and predict using the student_num model
student_num_01=as.numeric(Default$student)-1
glm.fit.student.num=glm(default~student_num_01,data=Default,family=binomial)
newy=predict(glm.fit.student.num,data.frame(student_num_01=c(0,1)),type="response")
newy

##Multiple Logistic Regression
#The following code builds a multiple logistic regression model
glm.fit_multi=glm(default~balance+income+student,data=Default,family=binomial)
summary(glm.fit_multi)
#Summary shows big p value for income, so unlikely to affect result
#also see apparent contradiction in ^B1 coefficient to the simple logistic regression results!

#Predicting the prob of default given a student with balance of 1500 and an income of 40k
predict(glm.fit_multi,data.frame(student="Yes",balance=1500,income=40000),type="response")
#result is 0.052, so result shown on slides is incorrect!