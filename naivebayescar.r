library(readr)
library(RCurl)

n_bayes_car<-function(train,test,train_label)
{
  test_d=dim(test)
  train_d=dim(train)
  #prior_probability
  p_prob=rep(0,4)
  for(i in 1:4)
  {
    p_prob[i] = as.numeric(table(train_label)[i]/length(train_label))
  }
  labels = c("acc","good","unacc","vgood")
  predicted_labels=rep(NA,nrow(test))
  for(i in 1:test_d[1])
  {
    #calulated_probabilty
    c_prob = matrix(ncol = 4,nrow = test_d[2])
    for(j in 1:test_d[2])
    {
      train_label_dat= cbind(train[,j],train_label)
      freq_table = table(train_label_dat)
      index = which(row.names(freq_table) == as.character(test[i,j]))
      nc = freq_table[index,]
      n = colSums(freq_table)
      m=4
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
#################
#split_car<-function(MyData)
#{
  MyData<-read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                   col_names = FALSE)
  a<-dim(MyData)
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
  train = list()
  test= list()
  for(j in 1:5)
  {
    f1=setdiff(flag1,j)
    test[[j]] = split_dat[[j]] 
    train[[j]] = rbind(split_dat[[f1[1]]],split_dat[[f1[2]]],split_dat[[f1[3]]],split_dat[[f1[4]]])
  }
#}

#split_car(MyData)

error_rate = rep(0,5)
for(i in 1:5){
  train_d = train[[i]]
  test_d = test[[i]]
  train_label = train_d[,7]
  test_label = test_d[,7]
  train_data = train_d[,1:6]
  test_data = test_d[,1:6]
  predict_label = n_bayes_car(train_data,test_data,train_label)
  error_rate[i] = mean(predict_label != test_label) * 100
}

plot(error_rate)

plot(error_rate,xlab = "5 fold validations",ylab = "Error rate",main = "Car Evaluation Dataset",type = "o")

