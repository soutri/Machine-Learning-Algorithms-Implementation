#install.packages("class")
library(class)
library(readr)
#k=5
#setwd("D:/R-progrms/assignment_4")
eu_final_matrix = matrix(ncol = 5,nrow = 5)
manh_final_matrix = matrix(ncol = 5,nrow = 5)
rpackage_matrix = matrix(ncol = 5,nrow = 5)
#------------------------------------------------------------------------------------------------------------
eu_distance<-function(test,train)
{
  distance<-apply(train,1,function(x)sqrt(sum((x-test)^2)))
  return(distance)
}
manhattan_distance<-function(test,train)
{
  distance<-apply(train,1,function(x)sum(abs(x-test)))
  return(distance)
}
knn_eu<-function(test,train,trainlabel,k)
{
  out_label<-rep(0,nrow(test))
  for(i in 1:nrow(test))
  {
    dis<-eu_distance(test[i],train)
    row_num<-which(dis %in% sort(dis)[1:k])
    row_label<-trainlabel[row_num]
    out_label[i]=names(which(table(row_label) == max(table(row_label))))
  }
  #print(mean(out_label == trainlabel)*100)
  return(out_label)
}
knn_manh<-function(test,train,trainlabel,k)
{
  out_label<-rep(0,nrow(test))
  for(i in 1:nrow(test))
  {
    dis<-manhattan_distance(test[i],train)
    row_num<-which(dis %in% sort(dis)[1:k])
    row_label<-trainlabel[row_num]
    out_label[i]=names(which(table(row_label) == max(table(row_label))))
  }
  #print(mean(out_label == orglabel)*100)
  return(out_label)
}
#--------------------------------------------------------------------------------------------------------------
MyData_raw<-read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                     col_names = FALSE)
a<-dim(MyData_raw)
flag1<-seq(1,5,1)
MyData=cbind(as.integer(factor(MyData_raw$X1)),as.integer(factor(MyData_raw$X2)),as.integer(factor(MyData_raw$X3)),as.integer(factor(MyData_raw$X4)),as.integer(factor(MyData_raw$X5)),as.integer(factor(MyData_raw$X6)),as.integer(factor(MyData_raw$X7)))
ind = seq(1,nrow(MyData),1)
x= sample(ind,replace = FALSE)
p=seq(345,1380,345)
split_dat=list()
for(i in 1:4)
{
  split_dat[[i]]=MyData[x[(p[i]-344):p[i]],]
}
split_dat[[5]]=MyData[x[1381:1728],]
train = list()
test= list()
for(j in 1:5)
{
  f1=setdiff(flag1,j)
  test[[j]] = split_dat[[j]] 
  train[[j]] = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
}

x = c(1,5,19,28,47)

row = 0
for(k in x){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    train_d = train[[i]]
    train_label = train_d[,7]
    train_data = train_d[,1:6]
    test_d = test[[i]]
    test_label = test_d[,7]
    test_data = test_d[,1:6]
    
    eu_new_label = knn_eu(test_data,train_data,train_label,k)
    error = mean(test_label != eu_new_label) * 100
    cat("Error1 = ",error,"\n")
    eu_final_matrix[row,i] = error
    
    manh_new_label = knn_manh(test_data,train_data,train_label,k)
    error2 = mean(test_label != manh_new_label) * 100
    cat("Error = ",error2,"\n")
    manh_final_matrix[row,i] = error2
    
  }  
}
rownames(eu_final_matrix) = c("K1","K5","K19","K28","K47")
rownames(manh_final_matrix) = c("K1","K5","K19","K28","K47")
boxplot(x = manh_final_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate for Manhattan Distance(Car)",xlim=c(1,5),ylim=c(1,50))
boxplot(x = eu_final_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate Euclidean Distance(car)",xlim=c(1,5),ylim=c(1,50))
#-------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------
x = c(1,5,19,28,47)

row = 0
for(k in x){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    train_d = train[[i]]
    train_label = train_d[,7]
    train_data = train_d[,1:6]
    test_d = test[[i]]
    test_label = test_d[,7]
    test_data = test_d[,1:6]
    new_label=knn(train_data,test_data,train_label, k = k, l = 0, prob = FALSE, use.all = TRUE)
    error11=mean(test_label != new_label) * 100
    rpackage_matrix[row,i]=error11
    boxplot(x = rpackage_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate for RPackage(Car)",xlim=c(1,5),ylim=c(1,50),type='o')
    #cat("Error1 = ",error,"\n")
    #eu_final_matrix[row,i] = error
  }  
}

















