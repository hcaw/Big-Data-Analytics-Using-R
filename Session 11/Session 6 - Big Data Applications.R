#firstly download the dataset
getwd()
algae <- read.table('Analysis.txt', header=F, dec='.',
                  col.names=c('season','size','speed','mxPH','mnO2','Cl','NO3','NH4','oPO4',
                  'PO4','Chla','a1','a2','a3','a4', 'a5','a6','a7'),
                  na.strings=c('XXXXXXX')) 

head(algae)
?head
summary(algae)
hist(algae$mxPH)
boxplot(algae$oPO4,ylab='Orthophosphate (oPO4)')
abline(h=mean(algae$oPO4,na.rm=T),lty=2)
rug(jitter(algae$oPO4),side=2)

###detect outliers with graphics
plot(algae$NH4,xlab='')
abline(h=mean(algae$NH4,na.rm=T),lty=1,col="red")
#anything above blue line is not normal
abline(h=mean(algae$NH4,na.rm=T)+sd(algae$NH4,na.rm=T),lty=2,col="blue")
abline(h=median(algae$NH4,na.rm=T),lty=3,col="green") 
#detect outliers with graphics
identify(algae$NH4) #<- doesn't work on mac!
algae[algae$NH4 >19000,] 

###data pre-processing
#removing the unknown obs.
library(DMwR)
data(algae)
algae[!complete.cases(algae),]
nrow(algae[!complete.cases(algae),])
algae <- na.omit(algae)

