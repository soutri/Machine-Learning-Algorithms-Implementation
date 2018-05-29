#install.packages("RCurl")
library(readr)
library(RCurl)
#setwd("D:/R-progrms/assignment_4")
k=5

kfold_ionosphere <- function(MyData,n)
{
  flag1<-seq(1,5,1)
  ind = seq(1,nrow(MyData),1)
  x= sample(ind,replace = FALSE)
  p=seq(70,350,70)
  split_dat=list()
  for(i in 1:4)
  {
    split_dat[[i]]=MyData[x[(p[i]-69):p[i]],]
  }
  split_dat[[5]]=MyData[x[281:351],]
  
  #splitdata <- split(MyData[1:nrow(MyData),],sample(rep(1:5,as.integer(nrow(MyData)/5))))
  
  for(i in 1:5)
  {
    f1=setdiff(flag1,i)
    test = split_dat[[i]] 
    train = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
    
    #x=paste(n,"_train_",i,".csv",sep = "")
    write.csv(test,paste(n,"_test_",i,".csv",sep = ""),row.names = FALSE)
  
    
    write.csv(train,paste(n,"_train_",i,".csv",sep = ""),row.names = FALSE)
  } 
}


ionosphere <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                       col_names = FALSE, col_types = cols(X2 = col_skip()))
#MyData1 <- ionosphere
#a1<-dim(MyData1)
n1<-"ionosphere"
kfold_ionosphere(ionosphere,n1)


kfold_car <- function(MyData,n)
{
  flag1<-seq(1,5,1)
  ind = seq(1,nrow(MyData),1)
  x= sample(ind,replace = FALSE)
  p=seq(345,1380,345)
  split_dat=list()
  for(i in 1:4)
  {
    split_dat[[i]]=MyData[x[(p[i]-344):p[i]],]
  }
  split_dat[[5]]=MyData[x[1381:1728],]
  
  #splitdata <- split(MyData[1:nrow(MyData),],sample(rep(1:5,as.integer(nrow(MyData)/5))))
  
  for(i in 1:5)
  {
    f1=setdiff(flag1,i)
    test = split_dat[[i]] 
    train = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
    
    #x=paste(n,"_train_",i,".csv",sep = "")
    write.csv(test,paste(n,"_test_",i,".csv",sep = ""),row.names = FALSE)
    
    
    write.csv(train,paste(n,"_train_",i,".csv",sep = ""),row.names = FALSE)
  } 
}

car <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                col_names = FALSE)
#MyData2 <- as.data.frame(myfile)
#a2<-dim(MyData2)
n2<-"car"
kfold_car(car,n2)


kfold_crx<- function(MyData,n)
{
  flag1<-seq(1,5,1)
  ind = seq(1,nrow(MyData),1)
  x= sample(ind,replace = FALSE)
  p=seq(138,690,138)
  split_dat=list()
  for(i in 1:5)
  {
    split_dat[[i]]=MyData[x[(p[i]-137):p[i]],]
  }
  
  #splitdata <- split(MyData[1:nrow(MyData),],sample(rep(1:5,as.integer(nrow(MyData)/5))))
  
  for(i in 1:5)
  {
    f1=setdiff(flag1,i)
    test = split_dat[[i]] 
    train = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
    
    #x=paste(n,"_train_",i,".csv",sep = "")
    write.csv(test,paste(n,"_test_",i,".csv",sep = ""),row.names = FALSE)
    
    
    write.csv(train,paste(n,"_train_",i,".csv",sep = ""),row.names = FALSE)
  } 
}

crx <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
                col_names = FALSE)
n3<-"crx"
kfold_crx(crx,n3)






