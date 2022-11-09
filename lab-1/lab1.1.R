install.packages("kknn")
install.packages("dplyr")
install.packages("Rfast")

data = read.csv("optdigits.csv")
library(dplyr)

#Divide data into training, validation, test in 50/25/25
set.seed(12345)
n=nrow(data)

trainId = sample(1:n, floor(n*0.5))
train = data[trainId, ]
notTrain = data[-trainId,]
train

n=nrow(notTrain)

testId = sample(1:n, floor(n*0.5))
test = notTrain[testId, ]
validation = notTrain[-testId, ]

library(kknn)
# Training data
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train, k=30, kernel = "rectangular")
Pred=ml$fitted.values
Prob=ml$prob #Probability of all classifications.
Prob
Pred
table((train[ ,ncol(train)]), Pred)


# 3.

library(Rfast)

# Finding out the two easiest eights to classify
Prob=ml$prob
Prob
Prob8=Prob[,9]
which.max(Prob8)
Prob8
Rfast:: nth(Prob8 , 2, num.of.nths=2, descending = TRUE, index.return = TRUE) # THe indexes for the highest three
Rfast:: nth(Prob8 , 2, num.of.nths=2, descending = TRUE) # The probability for the highest three
heatmapData=matrix(as.numeric(train[34,1:64]), nrow = 8, byrow = TRUE)
heatmapData
heatmap(heatmapData, Rowv= NA, Colv = NA)

# Finding out the three hardest eights to classify
Prob
# Column 9 is biggest of all columns
Prob
Prob8

train8=train[train[,65]==8, ]
train8[1,] train

ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train8, k=30, kernel = "rectangular")
Prob=ml$prob #Probability of all classifications.
# order
Prob
Prob8=Prob[,9]
which.min(Prob8)

Rfast:: nth(Prob8 , 3, num.of.nths=3, descending = FALSE, index.return = TRUE) # THe indexes for the highest three
Rfast:: nth(Prob8 , 3, num.of.nths=3, descending = FALSE) # The probability for the highest three
heatmapData=matrix(as.numeric(train8[202,1:64]), nrow = 8, byrow = TRUE)
heatmapData
heatmap(heatmapData, Rowv= NA, Colv = NA)

.# Test data 
?kknn
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


# Calculate the plot for finding out optimal K for the test data
accuracyVector = vector("numeric")

# X is target value, X1 contains the predictions, so for each instance we add the log product of target and predict
crossEntropy = function(X, X1){
  loss <- 0
  
  for (i in 1:nrow(X1)){
    loss=+ (log(X1[i, X[i,65]]+10^-15)) #Prob for the true class label X1
  }
  return (-loss)
}



for (n in 1:30) {
  ml = kknn(as.factor(train[ ,ncol(train)])~. , train, validation, k=n, kernel = "rectangular")
  
  # Calculate cross entropy ce
  ce=crossEntropy(validation,ml$prob)
  accuracyVector = c(accuracyVector, c(ce))
}

accuracyVector
plot(accuracyVector)
min(accuracyVector)
which.min(accuracyVector)

# Testing #############################





