require(ISLR)
library(MASS)
library(ggplot2)
MyData_raw=Auto

mpg01 <-rep(-1,nrow(MyData_raw))
dat_mean <-rep(-1,nrow(MyData_raw))
dat_sd <-rep(-1,nrow(MyData_raw))

for(b in 1:a[1])
{
  if(Auto$mpg[b]>median(Auto$mpg))
  {
    mpg01[b]=1
    
  }else
  {
    mpg01[b]=0
    
  }
  
}
new.Auto1=cbind(Auto$cylinders,Auto$displacement,Auto$horsepower,Auto$weight)

dat_mean=colMeans(new.Auto1)
dat_sd=sqrt(diag(var(new.Auto1)))

new.Auto1=scale(new.Auto1)
new.Auto=cbind(mpg01,1,new.Auto1)

MyData=new.Auto
a=dim(MyData)


e <- 1
m <- nrow(MyData)
SSE <-0
#cost_matrix is the partial derivative of SSE
cost_matrix <- matrix(0,nrow = 5,ncol=1)
theta_matrix <- matrix(0,nrow = 5,ncol=1)
sigmoid_matrix <- matrix(0,ncol=1,nrow=m)
sigmoid_matrix1 <- matrix(0,ncol=1,nrow=m)
hypothesis_matrix1 <- matrix(0,ncol=1,nrow=m)
iter <- 100
SSE_vect=matrix(0,nrow=iter,ncol=2)

for (e in 1:iter) {
  
  sigmoid_matrix = 1/(1+exp(-(MyData[,2:6] %*% theta_matrix)))
  #old_SSE=SSE
  SSE=(1/m) *(colSums(-MyData[,1]*log(sigmoid_matrix)-(1-MyData[,1])*log(1-sigmoid_matrix)))
  #dif=old_SSE-SSE
  SSE_vect[e,1] = e
  SSE_vect[e,2] = SSE
  
  
  print(SSE)
  #if(dif>0.01)
  #{
  #  break()
  #} 
  for(i in 1:5)
  {
    cost_matrix[i,1]=(1/m) * colSums(((sigmoid_matrix - MyData[,1]) * MyData[,i+1]))
  }
  for(j in 1:5)
  {
    theta_matrix[j,1] = theta_matrix[j,1] - (0.00003 * cost_matrix[j,1])
  }
  
  #print(e)
}

cat("the value of thetas from our model are",theta_matrix[1,],theta_matrix[2,],theta_matrix[3,],theta_matrix[4,],theta_matrix[5,])

#######Output from our algorithm#################

given = c(8,340,200,3500)

given = (given - dat_mean) / dat_sd
given = cbind(1,given)

predict = 0
for(i in 1:5){
  predict = predict + (theta_matrix[i,1] * given[i])
}

zz=1/(1+exp(-(predict)))

cat("The output for mpg01 is ",zz)



#############plot#################################
plot(SSE_vect[,1],SSE_vect[,2],xlab = "iterations",ylab = "cost",main="Logistic Regression with alpha=0.00003")
#plot(SSE_vect)
#abline(theta_matrix[1,], theta_matrix[2,])



###################Output from inbuilt function##################
#x1<-as.matrix(MyData[,2:5])
#y<-as.matrix(MyData[,1])

#theta<-as.matrix(solve(t(x1)%*%x1) %*% (t(x1)%*%y))
#output_2<-colSums(theta*x[1:7])
#print(output_2)


###########################3.1###############################
#-5,5,0.01
z=cbind(0,1,2,3,4,5,6,7,8,9)
#z=rnorm(10)
sigmoid = 1/(1+exp(-z))
plot(z,sigmoid,type="o",lty=1,main="Sigmoid function")

##########################3.3#####################################
sigmoid_matrix1 = 1/(1+exp(-(MyData[,2:6] %*% theta_matrix)))
label = rep(-1,length(sigmoid_matrix1))
for(i in 1:length(sigmoid_matrix1)){
  if(sigmoid_matrix1[i]>0.5){
    label[i] = 1
  }else if(sigmoid_matrix1[i]<0.5){
    label[i] = 0
  }
}

percent_error = (sum((MyData[,1] - label)^2)/length(sigmoid_matrix1))*100

cat("The percent_error of the model over new.Auto data set is",percent_error)
                                                              

