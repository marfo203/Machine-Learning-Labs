data = read.csv("optdigits.csv")
install.packages("dplyr")
library(dplyr)

#Divide data into training, validation, test in 50/25/25
set.seed(12345)
n=nrow(data)

trainId = sample(1:n, floor(n*0.5))
train = data[trainId, ]
train

n=nrow(notTrain)

testId = sample(1:n, floor(n*0.5))
test = notTrain[testId, ]
validation = notTrain[-testId, ]

install.packages("kknn")
library(kknn)
# Training data
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train, k=30, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((train[ ,ncol(train)]), Pred)

# Test data 
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, test, k=30, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((test[ ,ncol(test)]), Pred)



missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}
#missclass((train[ ,ncol(train)]), Pred) # Missclassification rate for training data
missclass((test[ ,ncol(test)]), Pred) # Missclassification rate for test data


head(attributes(Pred)$prob)

summary(ml)%>%filter(fit == 8)
?filter
filter(summary(ml), fit==8)
smry=tibble(summary(ml))%>%select_if(function(x) {})
ml


#Finding optimal K for test data

accuracyVector = vector("numeric")

missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}

# Calculate the plot for finding out optimal K for the test data
for (n in 1:30) {
    ml = kknn(as.factor(train[ ,ncol(train)])~. , train, test, k=n, kernel = "rectangular")
    Pred=ml$fitted.values
    mc = missclass((test[ ,ncol(test)]), Pred) # Missclassification rate for test data
   
  accuracyVector = c(accuracyVector, c(mc))
  
}
plot(accuracyVector)
min(accuracyVector)
which.min(accuracyVector)

# Missclassification rate train
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train, k=5, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((train[ ,ncol(train)]), Pred)



missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}
missclass((train[ ,ncol(train)]), Pred) # Missclassification rate for train data

# Missclassification rate test
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, test, k=5, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((test[ ,ncol(test)]), Pred)



missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}
missclass((test[ ,ncol(test)]), Pred) # Missclassification rate for test data

# Missclassification rate validation
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, validation, k=5, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((validation[ ,ncol(validation)]), Pred)



missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}
missclass((validation[ ,ncol(validation)]), Pred) # Missclassification rate for validation data


summary(ml)




#Part 1.5 calculate loss and find optimal K using cross entropy

# X is target value, X1 contains the predictions, so for each instance we add the log product of target and predict
crossEntropy = function(X, X1){
  loss <- 0
  
  for (i in X){
    loss <- loss + (X[i]*log(X1[i]))
  }
  return (-loss)
  
}

training = kknn(as.factor(train[ ,ncol(train)])~. , train, test, k=5, kernel = "rectangular")
validation=kknn(as.factor(train[ ,ncol(train)])~. , train, validation, k=5, kernel = "rectangular")
ceLoss = crossEntropy(training, validation)
ceLoss

training
