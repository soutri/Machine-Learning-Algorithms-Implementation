library(readr)
library(RCurl)

n_bayes_crx<-function(train,test,train_label)
{
  test_d=dim(test)
  train_d=dim(train)
  #prior_probability
  len = length(names(table(train_label)))
  p_prob=rep(0,len)
  for(i in 1:2)
  {
    p_prob[i] = as.numeric(table(train_label)[i]/length(train_label))
  }
  labels = c("-","+")
  predicted_labels=rep("0",nrow(test))
  for(i in 1:test_d[1])
  {
    #calulated_probabilty
    c_prob = matrix(ncol = 2,nrow = test_d[2])
    for(j in 1:test_d[2])
    {
      train_label_dat= cbind(train[,j],train_label)
      freq_table = table(train_label_dat)
      if(row.names(freq_table)[1] == "low"){
        text = c("low","middle","high")
        index = which(row.names(freq_table) == text[as.integer((test[i,j]))])  
      }
      else{
        index = which(row.names(freq_table) == as.character(test[i,j]))  
      }
      #index = which(row.names(freq_table) == as.character(test[i,j]))
      nc = freq_table[index,]
      n = colSums(freq_table)
      m=2
      for(c in 1:m)
      {
        c_prob[j,c] = (nc[c] + (m * 0.25)) / (n[c] + m)
      }
      
      
    }
    product = apply(c_prob,MARGIN = 2,FUN = prod)
    finalProb = product * p_prob
    ind = which.max(finalProb)
    predicted_labels[i] = labels[ind]
    
  }
  return(predicted_labels)
  
}

#####################################
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

rem=quantile(MyData$X2,c(0,1/3,2/3,1))
rem[1]=rem[1]-.00005
MyData$X2 = cut(MyData$X2, breaks=rem, labels=c("low","middle","high"))

rem=quantile(MyData$X3,c(0,1/3,2/3,1))
rem[1]=rem[1]-.00005
MyData$X3 = cut(MyData$X3, breaks=rem, labels=c("low","middle","high"))

rem=quantile(MyData$X8,c(0,1/3,2/3,1))
rem[1]=rem[1]-.00005
MyData$X8 = cut(MyData$X8, breaks=rem, labels=c("low","middle","high"))

rem=quantile(MyData$X11,c(0,1/3,2/3,1))
rem[1]=rem[1]-.00005
MyData$X11 = cut(MyData$X11, breaks=rem, labels=c("low","middle","high"))

rem=quantile(MyData$X14,c(0,1/3,2/3,1))
rem[1]=rem[1]-.00005
MyData$X14 = cut(MyData$X14, breaks=rem, labels=c("low","middle","high"))

rem=quantile(MyData$X15,c(0,1/3,2/3,1))
rem[1]=rem[1]-.00005
MyData$X15 = cut(MyData$X15, breaks=rem, labels=c("low","middle","high"))

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

error_rate = rep(0,5)
for(i in 1:5){
  #cat("Dataset - ",i)
  train_d = train[[i]]
  test_d = test[[i]]
  test_label = test_d[,16]
  train_label = train_d[,16]
  train_data = train_d[,1:15]
  test_data = test_d[,1:15]
  
  
  pred_labels = n_bayes_crx(train_data,test_data,train_label)
  error_rate[i] = mean(pred_labels != test_label) * 100
}

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Credit Approval Dataset",type = "o")


























