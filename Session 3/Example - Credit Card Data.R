#LOAD THE LIBRARIES
library(MASS)
library(ISLR)
#displays the data set in a table
fix(Default) #can't do this on my machine as no X11
?Default

#SCATTERPLOT INCOME V BALANCE
#no colours
plot(Default$balance,Default$income)
#with colours (takes a while)
plot(Default$balance,Default$income,col=ifelse(Default$default=='No',"blue","orange"),pch=ifelse(Default$default=='No',"o","+"))

#BOXPLOT DEFAULT V BALANCE
#no colours
plot(Default$default,Default$balance,xlab="Default",ylab="Balance")
#with colours
colours=c("blue","orange")
plot(Default$default,Default$balance,xlab="Default",ylab="Balance",col=colours)

#BOXPLOT DEFAULT V INCOME
#with colours
plot(Default$default,Default$income,xlab="Default",ylab="Income",col=colours)

#LINEAR REGRESSION - P(DEFAULT) V BALANCE
plot(Default$balance,Default$default,col="orange")
#we can see Default as numeric is stored as 1s and 2s
as.numeric(Default$default)
#turn No aka 1 to 0 & Yes aka 2 to 1
default_num<-as.numeric(Default$default)-1
default_num
#now the plot shows 0s and 1s
plot(Default$balance,default_num,col="orange")
#now we build the model (fit_linear) to plot the linear regression line
lm(default_num~balance,data=Default)
fit_linear<-lm(default_num~balance,data=Default)
summary(fit_linear)
abline(fit_linear,col="blue")

#LOGISTIC REGRESSION - P(DEFAULT) V BALANCE
glm.fit = glm(default~balance,data=Default,family=binomial)
abline(glm.fit) #not sure if we're supposed to do this...
summary(glm.fit)
#we will now plot the results of logistic regression
#Three ways to do it!

#1. Sort of cheating, as instead of smooth curve we plot the dots.
plot(Default$balance,glm.fit$fit,col="blue",pch=".")

#2. By using the inverse function of logit. 
#   This function will take ^B0+^B1.X as input and return p(X)
plot(Default$balance,default_num,col="orange") #simply p(default) v balance
#we take sample numbers from balance
#(create balance vector using 100 evenly spaced samples)
xrange<-seq(min(Default$balance),max(Default$balance),length.out=100)
library(boot) #no idea what this does
#plot the line using lines(x,y)
lines(xrange,inv.logit(glm.fit$coef[1]+glm.fit$coef[2]*xrange),col="red")

#3. Preferred Way - Making prediction first and plot the predicted value as a smooth line
y<-Default$default
x<-Default$balance
glm.fit_1<-glm(y~x,family=binomial)
#create a vector for yrange using predict
yrange<-predict(glm.fit_1,data.frame(x=xrange),type="response")
lines(xrange,yrange,col="red")

# -----

#HOW TO PREDICT USING THE LOGISTIC MODEL
#We predict the prob of default when an individual has an average balance of 1000 and 2000
glm.fit<-glm(default~balance,data=Default, family=binomial)
newy<-predict(glm.fit,data.frame(balance=c(1000,2000)),type="response")
newy
#We can see if balance==1000, p==0.0057 and if balance==2000, p==0.5857

#USING QUALITATIVE PREDICTORS FOR LOGISTIC REGRESSION (NO GRAPH!)
#The following code builds a logistic regression model between two qualitative variables.
glm.fit.student<-glm(default~student,data=Default,family=binomial)
summary(glm.fit_student)
#^B1 is positive - this indicates students tend to be more likely to default
#Following code predicts the prob given student or non-student
#Two ways of doing this!
#1. Use the student model directly (student model built from two qualitative variables)
newy<-predict(glm.fit.student,data.frame(student=c("No","Yes")),type="response")
newy
#We can see from above that students are far more likely to default
#2. Use student as a numeric value and predict using the student_num model.
student_num<-as.numeric(Default$student)-1
glm.fit.student.num<-glm(default~student_num,data=Default,family=binomial)
newy<-predict(glm.fit.student.num,data.frame(student_num=c(0,1)),type="response")
newy



