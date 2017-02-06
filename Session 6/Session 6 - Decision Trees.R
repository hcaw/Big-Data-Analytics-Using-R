####Decision Trees

###Regression Trees - Predicting a quantitative response

#Example - Baseball Player's Salaries
#Predict salary by regression tree based on Years & Hits
#Note - salary is measures in 1000s and log-transformed

#Step 1. Buiding the tree:
library(tree)
library(ISLR)
nrow(Hitters) #322
Hitters<-na.omit(Hitters) #remove rows with missing observations
nrow(Hitters) #263
tree.hitters<-tree(log(Salary)~Years+Hits, Hitters)
summary(tree.hitters)
plot(tree.hitters)
#pretty=0 includes the category names for any qualitative predictors, rather than simply displaying
#a letter for each category
text(tree.hitters,pretty=0)

#Step 2. Making Predictions
#Given Years=5 and Hits=100, what is the prediction?
yhat<-predict(tree.hitters,newdata = list("Years"=5,"Hits"=100))
yhat #5.688925
plot(Hitters$Years,Hitters$Hits) 

##Pruning Trees
#Before pruning:
tree.hitters<-tree(log(Salary)~Hits+Runs+RBI+Walks+Years+PutOuts+AtBat+Assists+Errors, data=Hitters)
plot(tree.hitters)
text(tree.hitters,pretty=0)
#My tree was different to hers.....no idea why!

##Pruning - Validation set
set.seed(2)
train<-sample(1:nrow(Hitters),132)
tree.hitters.train<-tree(log(Salary)~Hits+Runs+RBI+Walks+Years+PutOuts+AtBat+Assists+Errors, Hitters, subset=train)
plot(tree.hitters.train)
text(tree.hitters.train,pretty=0)
summary(tree.hitters.train)

##Pruning - Cross Validation
cv.hitters<-cv.tree(tree.hitters.train)
plot(cv.hitters$size,cv.hitters$dev,type='b')
#we can see from above that the size with the lowest MSE is 3
prune.hitters<-prune.tree(tree.hitters.train,best=3)
summary(prune.hitters)
plot(prune.hitters)
text(prune.hitters,pretty=0)

#Another way of visualisation
plot(Hitters$Years,Hitters$Hits,col="orange",pch=16,xlab="Years",ylab="Hits")
partition.tree(prune.hitters,ordvars = c("Years","Hits"),add = TRUE)

#Making predictions (with the pruned tree)
yhat<-predict(prune.hitters,newdata = Hitters[-train,])
hitters.test=log(Hitters[-train,"Salary"])
plot(yhat,hitters.test)
abline(0,1)
mean((yhat-hitters.test)^2) #0.4445136 - Test MSE for regression tree
sqrt(0.4445136) #0.6667185 - Square-root of MSE, which means this model leads to test predictions that are
                #within 1000*e^0.6667185=1947.8 of the true Salary of hitters

#If using the unpruned tree to do the same, the MSE is .....
yhat.unpruned<-predict(tree.hitters.train,newdata = Hitters[-train,])
mean((yhat.unpruned-hitters.test)^2) #...0.3746249
#Why does the unpruned tree have a lower MSE???

###Classification Trees

#Predicting a qualititative response

#Example: Carseats
library(ISLR)
#Sales is a continous variable - discretise it using ifelse()
High<-ifelse(Carseats$Sales<=8,"No","Yes")
#Merge High with the rest of the Carseats data
Carseats=data.frame(Carseats,High) #(adds one more column to Carseats)
#Fitting a classification tree
tree.carseats<-tree(High~.-Sales,Carseats)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats,pretty=0)

##Test Error Rate Estimation
#Estimate the test error rather than computing the training error
# - Split the observations into a training set and a data set
set.seed(2)
train<-sample(1:nrow(Carseats),nrow(Carseats)/2)
Carseats.test<-Carseats[-train,]
# - Build the tree using the training set
tree.carseats.train<-tree(High~.-Sales,data=Carseats,subset=train)
# - Evaluate its performance on the test data
tree.pred.test<-predict(tree.carseats.train,Carseats.test,type="class")
table(tree.pred.test,Carseats.test$High)
(30+27)/200
#28.5% Test Error Rate

##Calculate the Train Error Rate
High.train<-High[train]
tree.pred.train<-predict(tree.carseats.train,Carseats[train,],type="class")
table(tree.pred.train,High.train)
(12+6)/200
#9% Train Error Rate

##Pruning the Classification Tree
#Consider whether pruning the tree might lead to improved results

#Step 1. Use cv.tree() to determine the optimal level of tree complexity
set.seed(3)
cv.carseats<-cv.tree(tree.carseats.train,FUN=prune.misclass)
cv.carseats
#$size
#[1] 19 17 14 13  9  7  3  2  1
#$dev <----- this is the cv error rate
#[1] 55 55 53 52 50 56 69 65 80
#Above shows that the lowest cv error rate (50) is when the size == 9

#Step 2. Use prune.misclass() to prune the tree
prune.carseats<-prune.misclass(tree.carseats.train,best=9)

#Step 3. Performance Evaluation
tree.pred<-predict(prune.carseats,Carseats.test,type = "class")
table(tree.pred,Carseats.test$High)
(24+22)/200
#23% Test Error Rate --- better than without pruning (which was 28.5%)

#If we increase the value of the best, we obtain a larger pruned tree with lower classification accuracy
prune.carseats<-prune.misclass(tree.carseats.train,best=15)
tree.pred<-predict(prune.carseats,Carseats.test,type = "class")
table(tree.pred,Carseats.test$High)
(30+22)/200
#26% Test Error Rate, which is larger as expected
