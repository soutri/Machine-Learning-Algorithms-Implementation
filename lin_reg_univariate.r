#install.packages('plotly')
require(ISLR)
library(MASS)
library(ggplot2)
library(plotly)

raw<-Auto$horsepower
raw_mean=mean(raw)
raw_sd<-sqrt(var(raw))
MyData <-cbind(1,scale(raw),Auto$mpg)

e <- 1
SSE<-0
cost1 <- c()
cost2 <- c()
m <- nrow(MyData)

theta_matrix <- matrix(0,nrow=2,ncol=1)
hypothesis_matrix <- matrix(0,ncol=1,nrow=392)
iter <- 100
theta_plot <- matrix(0,nrow=iter,ncol=2)
cost_plot <- matrix(0,nrow = iter,ncol=1)

for (e in 1:iter) {
  hypothesis_matrix = MyData[,1:2] %*% theta_matrix
  SSE =(1/(2*m)) * colSums((hypothesis_matrix - MyData[,3])^2)
  print(SSE)
  cost_plot[e,1]=SSE
  
  cost1 = (1/m) * colSums(((hypothesis_matrix - MyData[,3]) * MyData[,1]))
  cost2 = (1/m) * colSums(((hypothesis_matrix - MyData[,3]) * MyData[,2]))
  
#cost.lms <- (1/(2*m)) * sum((hypothesis_matrix - MyData[,3])^2)
#cost_plot[e,1] <- e
# cost_plot[e,2] <- cost1
# cost_plot[e,3] <- cost2
  
  theta_matrix[1,] = theta_matrix[1,] - (0.1 * cost1)
  theta_matrix[2,] = theta_matrix[2,] - (0.1 * cost2)
  theta_plot[e,1] = theta_matrix[1,]
  theta_plot[e,2] = theta_matrix[2,]
  
 
}

cat("the values of theta are",theta_matrix[1,],"and",theta_matrix[2,])

plot(MyData[,2],MyData[,3],xlab = "horsepower",ylab = "mpg")
abline(theta_matrix[1,], theta_matrix[2,],col="red")

#contour_plot
cost.df <- data.frame(cost_plot)
xx<-as.data.frame(cbind(theta_plot[,1],theta_plot[,2],cost_plot[,1]))
plot_ly(data=xx,x=xx[,1],y=xx[,2],z=xx[,3], type = "contour", contours = list(showlabels = TRUE)) %>%
  colorbar(title = "Cost")

#1.3
dat1<-(220-raw_mean)/raw_sd
dat<-cbind(1,dat1)
hypothesis<-dat[1]*theta_matrix[1,]+dat[2]*theta_matrix[2,]
cat("output from our model",hypothesis)

#1.5
theta_matrix1=(ginv((t(MyData[,1:2]) %*% MyData[,1:2]))) %*% t(MyData[,1:2]) %*% MyData[,3]
abline(theta_matrix1[1,],theta_matrix1[2,],col="blue")
hypothesis1<-dat[1]*theta_matrix1[1,]+dat[2]*theta_matrix1[2,]
cat("the values of theta are",theta_matrix1[1,],"and",theta_matrix1[2,])
cat("output from normal equation",hypothesis1)



