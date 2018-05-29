library(readr)
library(RCurl)

n_bayes_ion<-function(train,test,train_label)
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
  labels = c("g","b")
  predicted_labels=rep("0",nrow(test))
  
  for(i in 1:test_d[1])
  {
    #calulated_probabilty
    c_prob = matrix(ncol = 2,nrow = test_d[2])
    for(j in 1:test_d[2])
    {
      train_label_dat= as.data.frame(cbind(train[,j],train_label))
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
        c_prob[j,c] = (nc[c] + (m * 0.5)) / (n[c] + m)
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

MyData_raw <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                       col_names = FALSE, col_types = cols(X1 = col_skip(), X2 = col_skip()))

test<-list()
train<-list()
convertToCategorical = function(dataColumn){
  b=quantile(dataColumn,c(0,1/3,2/3,1))
  b[1]=b[1]-.00005
  newColumn = cut(dataColumn, breaks=b, labels=c("low","middle","high"))
  return(newColumn)
}

MyDat = apply(X = MyData_raw[,1:32],MARGIN = 2,FUN = function(x)convertToCategorical(x))
MyData = cbind(MyDat,MyData_raw$X35)

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

for(i in 1:5)
{
  f1=setdiff(flag1,i)
  test[[i]] = split_dat[[i]] 
  train[[i]] = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
}

error_rate = rep(0,5)
for(i in 1:5){
  cat("Data ",i)
  train_dd = train[[i]]
  test_dd = test[[i]]
  test_label = test_dd[,33]
  train_label = train_dd[,33]
  train_data = train_dd[,1:32]
  test_data = test_dd[,1:32]
  
  
  pred_labels = n_bayes_ion(train_data,test_data,train_label)
  error_rate[i] = mean(pred_labels != test_label) * 100
}

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Ionosphere Dataset",type="o")


























