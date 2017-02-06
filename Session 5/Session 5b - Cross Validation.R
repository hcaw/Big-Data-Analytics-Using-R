####Cross Validation

###1. The Validation Set Approach

#Predict mpg from horsepower
#Randomly split Auto data set (392 obs.) into training (196 obs.) and 
#validation data (196 obs.).
#set the seed (will always be the same if you do this first!)
set.seed(1)
#selects random 196 numbers up to 392
train<-sample(392,196) 
#Fit the model using the training data set
lm.fit.train<-lm(mpg~horsepower,data=Auto,subset=train)
#Then, evaluate the model using the validation data set
mean((mpg-predict(lm.fit.train,Auto))[-train]^2)
#26.14142
#Plot the observations and linear relationship between mpg and horsepower
plot(mpg~horsepower,data=Auto,col="blue",main="Linear Regression: Validation Set Approach")
abline(lm.fit.train,col="red")

#Extra: show the Linear Regression model taken learnt from the entire training data set:
lm.fit<-lm(mpg~horsepower,data=Auto)
abline(lm.fit,col="black")

##A way to improve the above - polynomial!
#From the plot, there appears to be a non-linear relationship between mpg and horsepower.
#Try the quadratic model:   mpg~horsepower+horsepower2
#Repeat the procedure:

#Split the data as before and take a sample
set.seed(1)
train<-sample(392,196)
#Fit the model using the training data set:
lm.fit.quadratic.train<-lm(mpg~poly(horsepower,2),data=Auto,subset=train)
#Then evaluate the model using the validation data set
mean((mpg-predict(lm.fit.quadratic.train,Auto))[-train]^2)
#MSE = 19.82259
#The quadratic model has a smaller test error than the linear regression

##Plotting for different degrees of polynomial
#What occurs to MSE when we try the result with increasing degrees of polynomials
set.seed(1)
train<-sample(392,196)
errors<-rep(0,10)
errors[1]<-26.14142
for(i in 2:10){
  lm.fit.poly.train<-lm(mpg~poly(horsepower,i),data=Auto,subset=train)
  errors[i]<-mean((mpg-predict(lm.fit.poly.train,Auto))[-train]^2)
}
plot(errors,pch=16,col="red",xlab="Degree of Polynomial",ylab="Mean Squared Error", type="b",
     main="Variability in MSEs when plotting different Models")
#type='b' automatically adds in a broken line. If this is left out you can do
#lines(errors,col="red",type='b') to add it in after

##Repeating the validation method 10 times - random split each time!
#let's use continuous lines this time
#Now here's where we create the MSE matrix and plot
#First create an empty base plot
plot(0,col=1,pch=".",xlab="Degree of Polynomial",ylab="Mean Squared Error", type="l",
     main="Variability in MSEs when plotting different models",ylim=c(14,27),xlim=c(1,10))
errorMatrix<-matrix(nrow=10,ncol=10)
for(i in 1:10) {
  set.seed(i)
  train<-sample(392,196)
  for(j in 1:10){
    lm.fit.poly.train<-lm(mpg~poly(horsepower,j),data=Auto,subset=train)
    errorMatrix[i,j]<-mean((mpg-predict(lm.fit.poly.train,Auto))[-train]^2)
  }
  lines(errorMatrix[i,],col=i)
}

###2. Leave-One-Out Cross Validation (LOOCV)

#Using the Auto data set again, building a linear model
#Below the same as lm(mpg~horsepower,data=Auto)
glm.fit<-glm(mpg~horsepower,data=Auto)
#load the boot library (which contains cv.glm())
library(boot)
#cv.glm() does the LOOCV
cv.err<-cv.glm(Auto,glm.fit)
cv.err$delta
#The MSE is 24.23151

###3. k-fold Cross Validation
#same as above, then:
cv.err<-cv.glm(Auto,glm.fit,K=10)
cv.err$delta
#The MSE is 24.3120

###Auto Data: LOOCV vs k-fold CV
#First plot LOOCV
cv.error<-rep(0,10)
for(i in 1:10) {
  glm.fit<-glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]<-cv.glm(Auto,glm.fit)$delta[1]
}
plot(cv.error,col="blue",pch=16,xlab="Degrees of Polynomial",
     ylab = "Mean Squared Error",main="LOOCV",ylim=c(15,28),type='l')

#now plot k-fold CV (k=10)
plot(cv.error,col=1,pch=".",xlab="Degrees of Polynomial",
     ylab = "Mean Squared Error",main="10-fold CV",ylim=c(15,28),type='l')
cv.error.matrix<-matrix(nrow=9,ncol=10)

for(i in 1:9) {
  set.seed(i)
  for(j in 1:10) {
    glm.fit<-glm(mpg~poly(horsepower,j),data=Auto)
    cv.error.matrix[i,j]<-cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
  lines(cv.error.matrix[i,],col=i)
}