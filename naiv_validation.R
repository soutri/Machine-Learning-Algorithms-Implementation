#install.packages("naivebayes")
#install.packages("e1071")
library(naivebayes)
library(e1071)
library(readr)


MyData_raw <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/ionosphere/ionosphere.data", 
                       col_names = FALSE, col_types = cols(X1 = col_skip(), X2 = col_skip()))

convertToCategorical = function(dataColumn){
  b=quantile(dataColumn,c(0,1/3,2/3,1))
  b[1]=b[1]-.00005
  newColumn = cut(dataColumn,b,c("low","middle","high"))
  return(newColumn)
}

MyData1 = apply(X = MyData_raw[,1:32],MARGIN = 2,FUN = function(x)convertToCategorical(x))
MyData = cbind(MyData1,ionosphere$X35)

indices = seq(1,nrow(MyData),1)
temp = sample(x = indices,replace = FALSE)
point = seq(70,350,70)
split_data = list()
for(i in 1:4){
  split_data[[i]] = MyData[temp[(point[i]-69):point[i]],]
}
split_data[[5]] = MyData[temp[281:351],]

#Create train And Test Dataset

train = list()
test = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  test[[i]] = split_data[[i]]
  train[[i]] = rbind(split_data[[f[1]]],split_data[[f[2]]],split_data[[f[3]]],split_data[[f[4]]])
}


error_rate = rep(0,5)
for(i in 1:5){
  cat("Dataset - ",i)
  train_data = as.data.frame(train[[i]])
  test_data = as.data.frame(test[[i]])
  test_label = test_data$V33
  test_data = test_data[,1:32]
  output <- naiveBayes(V33 ~ ., data = train_data)
  preds <- predict(output, newdata = test_data)
  
  error_rate[i] = mean(preds != test_label) * 100
}

plot(error_rate,xlab = "k",ylab = "Error rate",main = "Naive Bayes Classifier(R Package)",sub = "Ionosphere Dataset",type='o')

#______________________________________________________________________________________________________________________________________

crx <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data", 
                col_names = FALSE, col_types = cols(X11 = col_number(), 
                                                    X14 = col_number()))
crx[crx == '?'] = NA
MyData = na.omit(crx)
MyData$X2 = as.double(MyData$X2)
MyData = MyData[-which(MyData$X7 == 'o'), ]
MyData = MyData[-which(MyData$X13 == 'p'), ]
MyData = MyData[-which(MyData$X4 == 'l'), ]

indices = seq(1,nrow(MyData),1)
temp = sample(x = indices,replace = FALSE)
point = seq(130,520,130)
split_data = list()
for(i in 1:4){
  split_data[[i]] = MyData[temp[(point[i]-129):point[i]],]
}
split_data[[5]] = MyData[temp[521:649],]
#Create train And Test Dataset

train = list()
test = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  test[[i]] = split_data[[i]]
  train[[i]] = rbind(split_data[[f[1]]],split_data[[f[2]]],split_data[[f[3]]],split_data[[f[4]]])
}

error_rate = rep(0,5)
for(i in 1:5){
  cat("Dataset - ",i)
  train_data = train[[i]]
  train_label = train_data[,16]
  test_data = test[[i]]
  test_label = test_data[,16]
  test_data = test_data[,1:15]
  
  output <- naive_bayes(formula = X16 ~ .,data = train_data)
  pred = predict(object = output,newdata = test_data)
  
  error_rate[i] = mean(as.character(pred) != test_label) * 100
}

plot(error_rate,xlab = "k",ylab = "Error rate",main = "Naive Bayes Classifier(R Package)",sub = "Credit Approval Dataset",type='o')


#--------------------------------------------------------------------------------------------------------------------------------------

car <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/car/car.data", 
                col_names = FALSE)
indices = seq(1,nrow(car),1)
temp = sample(x = indices,replace = FALSE)
point = seq(345,1380,345)
split_data = list()
for(i in 1:4){
  split_data[[i]] = car[temp[(point[i]-344):point[i]],]
}
split_data[[5]] = car[temp[1381:1728],]

#Create train And Test Dataset

train = list()
test = list()
flag = seq(1,5,1)
for(i in 1:5){
  f = setdiff(flag,i)
  test[[i]] = split_data[[i]]
  train[[i]] = rbind(split_data[[f[1]]],split_data[[f[2]]],split_data[[f[3]]],split_data[[f[4]]])
}


error_rate = rep(0,5)
for(i in 1:5){
  cat("Dataset - ",i)
  train_data = train[[i]]
  train_label = train_data[,7]
  test_data = test[[i]]
  test_label = test_data[,7]
  test_data = test_data[,1:6]
  
  output <- naive_bayes(formula = X7 ~ .,data = train_data)
  pred = predict(object = output,newdata = test_data)
  
  error_rate[i] = mean(as.character(pred) != test_label) * 100
}

plot(error_rate,xlab = "k",ylab = "Error rate",main ="Naive Bayes Classifier(R Package)",sub = "Car Evaluation Dataset",type = 'o')
