#5A

#How to calculate MSE in R (MSE is for regression problems)
library(ISLR)
fix(Auto)
#Using mpg for y & horsepower for x
lm.fit.Auto<-lm(mpg~horsepower,data=Auto)
#mean((y-^y)^2)
mean((Auto$mpg-predict(lm.fit.Auto,Auto))^2)

#How to calculate Error Rate in R (Error Rate is for classification problems)
fix(Default)
#build the glm.fit
glm.fit<-glm(default~income+balance+student,data=Default,family=binomial)
summary(glm.fit)
#making predictions as probabilities (using glm.fit)
glm.probs<-predict(glm.fit,type="response")
glm.probs[1:10]
#the contrasts function indicates that R has created a dummy variable with a 1 for Yes
contrasts(Default$default)
length(Default$default)
#create a vector for predicting yes or no. It has the same length as Default$default and has "No" as the initial values
glm.pred<-rep("No",10000)
#set those whose prob > 0.5 to be "Yes"
glm.pred[glm.probs>.5]="Yes"
#The table function shows the confusion matrix
table(glm.pred,Default$default)
#using mean to show the accuracy and error rate (both below add up to 1)
mean(glm.pred==Default$default)
mean(glm.pred!=Default$default)

# ----------------------------

#5B

#How to split data
#Should set seed if you want randomised data to be repeatable?
set.seed(1)
#randomly split auto data set (392 obs) into training (196 obs) and validation data (196 obs)
train<-sample(392,196)
#Fit the model using the training data set
lm.fit.train<-lm(mpg~horsepower,data=Auto,subset=train)
#Then, evaluate the model using the validation data set
mean((Auto$mpg-predict))