install.packages("kknn")
install.packages("dplyr")
install.packages("Rfast")

# 1. Divide data into training, validation, test in 50/25/25
library(dplyr)
data = read.csv("optdigits.csv")
set.seed(12345)
n=nrow(data)
id = sample(1:n, floor(n*0.5))
train = data[id, ]

id1 = setdiff(1:n, id)
set.seed(12345)
id2 = sample(id1, floor(n*0.25))
valid = data[id2, ]

id3 = setdiff(id1, id2)
test = data[id3, ]

####### Previous solution
trainId = sample(1:n, floor(n*0.5))
train = data[trainId, ]
notTrain = data[-trainId,]

n=nrow(notTrain)

testId = sample(1:n, floor(n*0.5))
test = notTrain[testId, ]
validation = notTrain[-testId, ]
########


# 2.
library(kknn)
# Missclassification 
missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}

# Train data
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train, k=30, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((train[ ,ncol(train)]), Pred)

missclass((train[ ,ncol(train)]), Pred) # Missclassification rate for training data

# Test data 
ml = kknn(as.factor(train[ ,ncol(train)])~., train, test, k=30, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((test[ ,ncol(test)]), Pred)

missclass((test[ ,ncol(test)]), Pred) # Missclassification rate for test data

# 3.

library(Rfast)

# Finding out the two easiest eights to classify

# Train data
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train, k=30, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((train[ ,ncol(train)]), Pred)

Prob=ml$prob
Prob
Prob8=Prob[,9]
which.max(Prob8)
Prob8
Rfast:: nth(Prob8 , 2, num.of.nths=2, descending = TRUE, index.return = TRUE) # The indexes for the highest three
Rfast:: nth(Prob8 , 2, num.of.nths=2, descending = TRUE) # The probability for the highest three
heatmapData=matrix(as.numeric(train[644,1:64]), nrow = 8, byrow = TRUE)
heatmap(heatmapData, Rowv= NA, Colv = NA)

# Finding out the three hardest eights to classify
# Column 9 is biggest of all columns
train8=train[train[,65]==8, ]
train8[1,]
train

ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train8, k=30, kernel = "rectangular")
Prob=ml$prob #Probability of all classifications.
# order
Prob
Prob8=Prob[,9]
which.min(Prob8)

Rfast:: nth(Prob8 , 3, num.of.nths=3, descending = FALSE, index.return = TRUE) # THe indexes for the highest three
Rfast:: nth(Prob8 , 3, num.of.nths=3, descending = FALSE) # The probability for the highest three
heatmapData=matrix(as.numeric(train8[202,1:64]), nrow = 8, byrow = TRUE)
heatmap(heatmapData, Rowv= NA, Colv = NA)

# 4. Finding optimal K for test data



missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}

# Calculate the plot for finding out optimal K for the test data
accuracyVectorValid = vector("numeric")
accuracyVectorTrain = vector("numeric")

for (n in 1:30) {
    ml = kknn(as.factor(train[ ,ncol(train)])~. , train, valid, k=n, kernel = "rectangular")
    Pred=ml$fitted.values
    mc = missclass((valid[ ,ncol(valid)]), Pred) # Missclassification rate for valid data
    
    accuracyVectorValid = c(accuracyVectorValid, c(mc))
    
    ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train, k=n, kernel = "rectangular")
    Pred=ml$fitted.values
    mc = missclass((train[ ,ncol(train)]), Pred) # Missclassification rate for train data
    
    accuracyVectorTrain = c(accuracyVectorTrain, c(mc))
  
}
plot(1:30, accuracyVectorTrain, ylab = "missclassification_rate", xlab = "k_value",col = "purple")
points(1:30, accuracyVectorValid, col="green")
accuracyVectorValid ### 3, 9, 10, 11
min(accuracyVectorValid)
which.min(accuracyVectorValid)

# Missclassification rate train
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, train, k=10, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((train[ ,ncol(train)]), Pred)

missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}
missclass((train[ ,ncol(train)]), Pred) # Missclassification rate for train data

# Missclassification rate valid
ml = kknn(as.factor(train[ ,ncol(train)])~. , train, valid, k=10, kernel = "rectangular")
Pred=ml$fitted.values
Pred
table((valid[ ,ncol(valid)]), Pred)

missclass = function(X, X1){
  n = length(X)
  return(1 - sum(diag(table(X, X1)))/n)
  
}
missclass((valid[ ,ncol(valid)]), Pred) # Missclassification rate for test data

summary(ml)

#Part 1.5 calculate loss and find optimal K using cross entropy

# Calculate the plot for finding out optimal K for the test data
accuracyVector = vector("numeric")

# X is target value, X1 contains the predictions, so for each instance we add the log product of target and predict
crossEntropy = function(X, X1){
  loss <- 0
  for (i in 1:nrow(X1)){
    loss = loss + (log(X1[i, X[i,65]+1]+10^-15)) #Prob for the true class label X1
  }
  return (-loss)
}

for (n in 1:30) {
  ml = kknn(as.factor(train[ , ncol(train)])~. , train, valid, k=n, kernel = "rectangular")
  
  # Calculate cross entropy ce
  ce=crossEntropy(valid, ml$prob)
  accuracyVector = c(accuracyVector, c(ce))
}

accuracyVector
plot(accuracyVector)
min(accuracyVector)
which.min(accuracyVector)

# Testing #############################

