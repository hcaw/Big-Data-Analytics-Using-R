library(ISLR)
library(MASS)

x=c(10, 2, 15, 6, 4, 9, 12, 11, 3, 0, 12, 10, 9, 7, 11, 10, 8, 5, 10, 6)
table(x)
x=rnorm(50)

fert=c(4,6,10,12)
yield=c(3.0,5.5,6.5,9.0)
FertYield=data.frame(fert,yield)
lm.fit.Fert=lm(yield~fert,data=FertYield)
plot(fert,yield)
abline(lm.fit.Fert)

predict(lm.fit.Fert,newdata=data.frame(fert=c(2.5,5.5,8.5))) 

plot(Default$default,Default$balance,xlab="Default",ylab="Balance")


plot(Default$balance,Default$income)
plot(Default$balance,Default$income,col=ifelse(Default$default=='No',"blue","orange"),pch=ifelse(Default$default=='No',"o","+"))

colors=c("blue","orange")
plot(Default$default,Default$balance,xlab="Default",ylab="Balance",col=colors)
plot(Default$balance,Default$default,col="orange")
as.numeric(Default$default)

default_num=as.numeric(Default$default)-1
fit_linear=lm(default_num~balance,data=Default)
abline(fit_linear,col="blue")

student_num_01=as.numeric(Default$student)-1
glm.fit.01=glm(default~student_num_01, data=Default, family=binomial)
newy=predict(glm.fit.01, data.frame(student_num_01=c(0,1)), type="response")


y=Default$default
x=Default$balance
glm.fit_1=glm(y~x,family=binomial)
xrange=seq(min(Default$balance),max(Default$balance),length.out=100)
yrange<-predict(glm.fit_1,data.frame(x=xrange),type="response")
default_num<-as.numeric(Default$default)-1
plot(Default$balance,default_num,col="orange")
lines(xrange,yrange,col="red")


glm.fit_multi<-glm(default~balance+income+student,data=Default,family=binomial)
summary(glm.fit_multi)
predict(glm.fit_multi, data.frame(student="Yes",balance=1500,income=40000), type="response")
