###How to Calculate MSE in R

#Given the Auto data set, we will calculate its training MSE
library(ISLR)
attach(Auto)
lm.fit<-lm(mpg~horsepower,data=Auto)
mean((mpg-predict(lm.fit,Auto))^2)
#Training MSE is 23.94

###How to Calculate Error Rate in R?

#Multiple Logistic Regression first
attach(Default)
glm.fit<-glm(default~income+balance+student,data=Default,family=binomial)
summary(glm.fit)

#create predictions as probabilities
glm.probs<-predict(glm.fit,type="response")
glm.probs[1:10]

#The contrasts function below indicates that R has created a dummy variable with a 1 for Yes
contrasts(default)
length(default)

#Create a vector for predicting yes or no. It has the same length as Default$default, 
#and has "No" as the initial values
glm.pred<-rep("No",length(default))
#set prob >.5 to "Yes"
glm.pred[glm.probs>.5]="Yes"

#The table function shows the confusion matrix
table(glm.pred,default)

#Using mean to show the accuracy and error rate
mean(glm.pred==default)
#accuracy = 0.9732
mean(glm.pred!=default)
#error rate = 0.0268