#Below creates 100 points with a normal distribution  (50 rows and 2 columns)
x<-matrix(rnorm(50*2),ncol=2)   
x[1:25,1]<-x[1:25,1]+3
x[1:25,2]<-x[1:25,2]-4 

#slide 47 (or extra sheet) - we should also try single and average linkage but we cannot do centroid linkage 
#(as we know distance here but NOT co-ordinates)