library(readr)
#k=5
#install.packages("class")
library(class)
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
#----------------------------------------------------------------------------------------------------
MyData_raw<- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
                      col_names = FALSE, col_types = cols(X11 = col_number(), 
                                                          X14 = col_number()))
test<-list()
train<-list()
MyData_raw[MyData_raw == '?'] = NA
MyData = na.omit(MyData_raw)
MyData$X2 = as.double(MyData$X2)
MyData = MyData[-which(MyData$X7 == 'o'), ]
MyData = MyData[-which(MyData$X13 == 'p'), ]
MyData = MyData[-which(MyData$X4 == 'l'), ]

MyData$X1 = as.integer(factor(MyData$X1))
MyData$X4 = as.integer(factor(MyData$X4))
MyData$X5 = as.integer(factor(MyData$X5))
MyData$X6 = as.integer(factor(MyData$X6))
MyData$X7 = as.integer(factor(MyData$X7))
MyData$X9 = as.integer(factor(MyData$X9))
MyData$X10 = as.integer(factor(MyData$X10))
MyData$X12 = as.integer(factor(MyData$X12))
MyData$X13 = as.integer(factor(MyData$X13))

flag1<-seq(1,5,1)
ind = seq(1,nrow(MyData),1)
x= sample(ind,replace = FALSE)
p=seq(130,520,130)
split_dat=list()
for(i in 1:4)
{
  split_dat[[i]]=MyData[x[(p[i]-129):p[i]],]
}
split_dat[[5]] = MyData[x[521:649],]

#splitdata <- split(MyData[1:nrow(MyData),],sample(rep(1:5,as.integer(nrow(MyData)/5))))

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
    train_label = train_d[,16]
    train_data = train_d[,1:15]
    test_d = test[[i]]
    test_label = test_d[,16]
    test_data = test_d[,1:15]
    
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
boxplot(x = manh_final_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate for Manhattan Distance(CRX)",xlim=c(1,5),ylim=c(1,50))
boxplot(x = eu_final_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate Euclidean Distance(CRX)",xlim=c(1,5),ylim=c(1,50))

#--------------------------------------------------------------------------------------

x = c(1,5,19,28,47)

row = 0
for(k in x){
  cat("K = ",k,"\n")
  row = row + 1
  for(i in 1:5){
    train_d = train[[i]]
    train_label = as.matrix(train_d[,16])
    train_data = as.matrix(train_d[,1:15])
    test_d = test[[i]]
    test_label = as.matrix(test_d[,16])
    test_data = test_d[,1:15]
    
    new_label=as.matrix(knn(train_data,test_data,cl=train_label,k = k, l = 0, prob = FALSE, use.all = TRUE))
    #knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
    error11=mean(test_label != new_label) * 100
    cat("Error11 = ",error11,"\n")
    rpackage_matrix[row,i] = error11
    boxplot(x = rpackage_matrix,use.cols = FALSE,xlab = "K",ylab = "% Error",main="Error rate for R Package(CRX)",xlim=c(1,5),ylim=c(1,50))
   
    
  }  
}
#---------------------------------------------------------------------------------------------------------


