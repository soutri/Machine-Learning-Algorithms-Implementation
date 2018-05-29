library(readr)
#install.packages("class")
library(class)
#k=5
setwd("D:/R-progrms/assignment_4")
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
    print(i)
    dis<-eu_distance(test[i,],train)
    row_num<-which(dis %in% sort(dis)[1:k])
    row_label<-trainlabel[row_num,]
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
    dis<-manhattan_distance(test[i,],train)
    row_num<-which(dis %in% sort(dis)[1:k])
    row_label<-trainlabel[row_num,]
    out_label[i]=names(which(table(row_label) == max(table(row_label))))
  }
  #print(mean(out_label == orglabel)*100)
  return(out_label)
}

#----------------------------------------------------------------------------------------------------------
MyData <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                       col_names = FALSE, col_types = cols(X1 = col_skip(), X2 = col_skip()))

flag1<-seq(1,5,1)
ind = seq(1,nrow(MyData),1)
x= sample(ind,replace = FALSE)
p=seq(70,350,70)
split_dat=list()
for(i in 1:4)
{
  split_dat[[i]]=MyData[x[(p[i]-69):p[i]],]
}
split_dat[[5]] = MyData[x[281:351],]

#splitdata <- split(MyData[1:nrow(MyData),],sample(rep(1:5,as.integer(nrow(MyData)/5))))
train = list()
test= list()
for(i in 1:5)
{
  f1=setdiff(flag1,i)
  test[[i]] = split_dat[[i]] 
  train[[i]] = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
}

x = c(1,5,19,28,47)

row = 0
for(k in x){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    train_d = train[[i]]
    train_label = train_d[,33]
    train_data = train_d[,1:32]
    test_d = test[[i]]
    test_label = test_d[,33]
    test_data = test_d[,1:32]
    
    eu_new_label = knn_eu(test_data,train_data,train_label,k)
    error = mean(test_label != eu_new_label) * 100
    cat("Error1 = ",error,"\n")
    eu_final_matrix[row,i] = error
    
    manh_new_label = knn_manh(test_data,train_data,train_label,k)
    error2 = mean(test_label != manh_new_label) * 100
    cat("Error2 = ",error2,"\n")
    manh_final_matrix[row,i] = error2
    
  }  
}
rownames(eu_final_matrix) = c("K1","K5","K10","K15","K20")
rownames(manh_final_matrix) = c("K1","K5","K10","K15","K20")
boxplot(x = manh_final_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate for Manhattan Distance(Ionosphere)",xlim=c(1,5),ylim=c(1,50))
boxplot(x = eu_final_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate Euclidean Distance(Ionosphere)",xlim=c(1,5),ylim=c(1,50))
#--------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
x = c(1,5,19,28,47)

row = 0
for(k in x){
 cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    train_d = train[[i]]
    train_label = as.matrix(train_d[,33])
    train_data = as.matrix(train_d[,1:32])
    test_d = test[[i]]
    test_label = test_d[,33]
    test_data = test_d[,1:32]
    new_label=as.matrix(knn(train_data,test_data,train_label,k = k,l = 0,prob = FALSE,use.all = TRUE))
    error11=mean(test_label != new_label) * 100
    rpackage_matrix[row,i]=error11
    boxplot(x = rpackage_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate for RPackage(Ionosphere)",xlim=c(1,5),ylim=c(1,50))
    
    
 }  
}

