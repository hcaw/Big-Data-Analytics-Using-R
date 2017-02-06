###Support Vector Classifiers
set.seed(1)
x<-matrix(rnorm(20*2),ncol=2)
x
y<-c(rep(-1,10),rep(1,10))
y
#for rows 11 to 20, add one
x[y==1,]<-x[y==1,]+1
x
plot(x,col=(3-y))
palette()
#encode the response as a factor variable by creating a data frame:
dat<-data.frame(x=x,y=as.factor(y))
library(e1071)
svmfit<-svm(y~.,data=dat,kernel="linear",cost=10,scale=FALSE)
#support vectors = cross, remaining = circle
plot(svmfit,data=dat)
#7 support vectors:
svmfit$index
summary(svmfit)
#try a smaller cost
svmfit<-svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)
plot(svmfit,data=dat)
#16 support vectors:
svmfit$index

##tune() - perform 10-fold cross-validation
#another example of using CV to compare and select model
set.seed(1)
tune.out<-tune(svm,y~.,data=dat,kernel="linear",ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
bestmod<-tune.out$best.model
summary(bestmod)

##predict class labels
#first generate a test data set
xtest<-matrix(rnorm(20*2),ncol=2)
ytest<-sample(c(-1,1),20,rep=TRUE)
ytest
xtest
xtest[ytest==1,]<-xtest[ytest==1,]+1
testdat<-data.frame(x=xtest,y=as.factor(ytest))
testdat
#then predict the class labels of these test observations
# - first using the best model (with cost=0.1)
ypred<-predict(bestmod,testdat)
#with cost=0.1, 19 of the test observations are correctly classified.
table(predict=ypred,truth=testdat$y)
#-what if cost=0.01?
svmfit<-svm(y~.,data=dat,kernel="linear",cost=0.01,scale=FALSE)
ypred<-predict(svmfit,testdat)
#with cost=0.01, 18 of the test observations are correctly classified.
table(predict=ypred,truth=testdat$y)

##A linearly separable example
#First generate a linearly seperable training set (repeated until success)
x<-matrix(rnorm(20*2),ncol=2)
y<-c(rep(-1,10),rep(1,10))
#used +1 below as couldn't get +0.5 to work.
x[y==1,]<-x[y==1]+1
plot(x,col=(y+5)/2,pch=19)

#we fit the SVC and plot the resulting hyperplane, using a very large 
#value of cost so that no observations are misclassified
dat<-data.frame(x=x,y=as.factor(y))
svmfit<-svm(y~.,data=dat,kernel="linear",cost=1e5)
summary(svmfit)
plot(svmfit,dat)
#only 3 support vectors used, margin very narrow - however some circle
#observations are very close to the decision boundary.
#It seems that this model will perform poorly on test data.
#We now generate a test dataset and calculate the error rate:
# - TODO!

#Slide 31 - also TODO