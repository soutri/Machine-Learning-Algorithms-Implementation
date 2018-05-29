require(ISLR)
library(MASS)
library(ggplot2)


dat_mean=colMeans(Auto[2:7])
dat_sd=sqrt(diag(var(Auto[2:7])))
MyData_raw<-scale(cbind(Auto[2:7]))

MyData<-as.matrix(cbind(1,MyData_raw,Auto[1]))
dif=0

e <- 1
m <- nrow(MyData)
#cost_matrix is the partial derivative of SSE
cost_matrix <- matrix(0,nrow = 7,ncol=1)
theta_matrix <- matrix(0,nrow=7,ncol=1)
hypothesis_matrix <- matrix(0,ncol=1,nrow=m)
hypothesis_matrix1 <- matrix(0,ncol=1,nrow=m)
#theta_plot <- matrix(0,nrow=iter,ncol=2)
cost_plot <- matrix(0,nrow = iter,ncol=2)
iter <- 1000

for (e in 1:iter) {
  
  hypothesis_matrix = MyData[,1:7] %*% theta_matrix
  old_SSE=SSE
  SSE =(1/(2*m)) * colSums((hypothesis_matrix - MyData[,8])^2)
  cost_plot[e,1]=e
  cost_plot[e,2]=SSE
  dif=old_SSE-SSE
  print(dif)
  #if(dif>0.01)
  #{
   # break()
  #} 
  for(i in 1:7)
  {
    cost_matrix[i,1]=(1/m) * colSums(((hypothesis_matrix - MyData[,8]) * MyData[,i]))
  }
  for(j in 1:7)
  {
    theta_matrix[j,1] = theta_matrix[j,1] - (0.1 * cost_matrix[j,1])
  }
  
  #print(e)
}

cat("the value of thetas from our model are",theta_matrix[1,],theta_matrix[2,],theta_matrix[3,],theta_matrix[4,],theta_matrix[5,],theta_matrix[6,],theta_matrix[7,])


#######Output from our algorithm#################
x<-as.matrix(cbind(4, 300, 200, 3500, 11, 70, 2))
x=x-dat_mean
x=x/dat_sd
x=cbind(1,x)
a=colSums(theta_matrix*x[1:7])
cat("Output from our model is",a)



#############plot#################################
#plot(MyData[,7],MyData[,8],xlab = "horsepower",ylab = "mpg")
#abline(theta_matrix[1,], theta_matrix[2,])
plot(cost_plot[,1],cost_plot[,2],xlab = "iterations",ylab = "cost",main = "Multivariate LM with alpha=0.00003")

###################Output from normal function##################
x1<-as.matrix(MyData[,1:7])
y<-as.matrix(MyData[,8])

#theta<-as.matrix(solve(t(x1)%*%x1) %*% (t(x1)%*%y))
theta_matrix1=(ginv((t(MyData[,1:7]) %*% MyData[,1:7]))) %*% t(MyData[,1:7]) %*% MyData[,8]
cat("the value of thetas from normal equation are",theta_matrix[1,],theta_matrix[2,],theta_matrix[3,],theta_matrix[4,],theta_matrix[5,],theta_matrix[6,],theta_matrix[7,])
output_2<-colSums(theta_matrix1*x[1:7])
cat("output from normal equation is",output_2)



