# install.packages("tree")

# 1. 
data = read.csv2("bank-full.csv", stringsAsFactors = TRUE) # We use read.csv2 since the delimitor is ";"
?read.csv2()
data = as.data.frame(data%>%select(-duration))
str(data) # Checking the types of the data
# Converting character into factor
# I need to find out how to not have to do this manually!
data$y = as.factor(data$y)
data$job = as.factor(data$job)
data$marital = as.factor(data$marital)
data$education = as.factor(data$education)
data$default = as.factor(data$default)
data$housing = as.factor(data$housing)
data$loan = as.factor(data$loan)
data$contact = as.factor(data$contact)
data$month = as.factor(data$month)
data$poutcome = as.factor(data$poutcome)

# Dividing the data into train, valid and test
library(dplyr)
set.seed(12345)
n=nrow(data)
train.id = sample(1:n, floor(n*0.4))
train = data[train.id, ]

temp.id = setdiff(1:n, train.id)
set.seed(12345)
valid.id = sample(temp.id, floor(n*0.3))
valid = data[valid.id, ]

test.id = setdiff(temp.id, valid.id)
test = data[test.id, ]

# 2. Fit decision trees to the training data so that you change the default settings
# one by one (i.e. not simultaneously):
library(tree)
# a. Decision Tree with default settings. 
fit = tree(y~., data=train)
summary(fit)
plot(fit)
text(fit, pretty = 0)

# Misclassification rate training
Yfit = predict(fit, newdata = train, type = "class")
missclass.matrix = table(train$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for training data: ", missclass.rate))

# Misclassification rate validation
Yfit = predict(fit, newdata = valid, type = "class")
missclass.matrix = table(valid$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for validation data: ", missclass.rate))

# b. Decision Tree with smallest allowed node size equal to 7000.
?tree
fit = tree(y~., data=train, control = tree.control(nrow(train), minsize = 7000))
summary(fit) # This is the best one, same misclassification rate but a smaller less complex model.
plot(fit)
text(fit, pretty = 0)
# Misclassification rate training
Yfit = predict(fit, newdata = train, type = "class")
missclass.matrix = table(train$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for training data: ", missclass.rate))

# Misclassification rate validation
Yfit = predict(fit, newdata = valid, type = "class")
missclass.matrix = table(valid$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for validation data: ", missclass.rate))


# c. Decision trees minimum deviance to 0.0005.
fit = tree(y~., data=train, control = tree.control(nrow(train), mindev = 0.0005))
plot(fit)
text(fit, pretty = 0)
summary(fit)

# Misclassification rate training
Yfit = predict(fit, newdata = train, type = "class")
missclass.matrix = table(train$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for training data: ", missclass.rate))

# Misclassification rate validation
Yfit = predict(fit, newdata = valid, type = "class")
missclass.matrix = table(valid$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for validation data: ", missclass.rate))

# 3. 
fit = tree(y~., data=train, control = tree.control(nrow(train), mindev = 0.0005))
trainScore=rep(0,49)
testScore=rep(0,49)
for(i in 2:50) {
  prunedTree=prune.tree(fit,best=i)
  pred=predict(prunedTree, newdata=valid, type="tree")
  trainScore[i-1]=deviance(prunedTree)
  testScore[i-1]=deviance(pred)
}
plot(2:50, trainScore[2:50], type="b", col="red", ylim=c(8000,12000))
points(2:50, testScore[2:50], type="b", col="blue")
plot(2:50, testScore[2:50], type="b", col="blue")
min(testScore)
which.min(testScore) # 21 nodes is king

# 4. 
finalTree=prune.tree(fit, best=21)
Yfit=predict(finalTree, newdata=test, type="class")
missclass.matrix = table(test$y, Yfit)
missclass.matrix
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for test data: ", missclass.rate))
print(paste("Accuracy for test data: ", 1 - missclass.rate))

# F1 = 2 * (Precision * Recall) / (Precision + Recall)
library(caret)
precision(missclass.matrix)
# Precision = True Positive / (True Positive + False Positive)
F1.precision = missclass.matrix[2,2] / (missclass.matrix[2,2] + missclass.matrix[1,2]) 
# Recall = True Positive / (True Positive + False Negative)
F1.recall = missclass.matrix[2,2] / (missclass.matrix[2,2] + missclass.matrix[2,1]) 
F1 = 2 * (F1.precision * F1.recall) / (F1.precision + F1.recall) # Good on predicting no, bad on predicting yes

# 5. 
fit = tree(y~., data=train, control = tree.control(nrow(train), mindev = 0.0005), loss = loss.matrix) # Should we do a normal fit?
loss.matrix = matrix(c(0, 1,  5, 0), nrow = 2, byrow=TRUE)
finalTree = prune.misclass(fit, loss = loss.matrix, best = 21)
summary(finalTree)
Yfit = predict(finalTree, newdata = test, type = "class")
confusion.matrix = table(test$y, Yfit)
confusion.matrix
loss.matrix
Yfit
# 6. 
fit = tree(y~., data=train, control = tree.control(nrow(train), mindev = 0.0005))
finalTree=prune.tree(fit, best=21)
Yfit=predict(finalTree, newdata=test)
r = 0.05
while (r < 1) {
Pred = sapply(Yfit[, 2], function(x) ifelse(x > r, 'yes', 'no'))
confusion.matrix = table(test$y, Pred)
confusion.matrix
# TPR True positive rate
TPR = c(TPR, confusion.matrix[2,2])
# FPR False positive rate
FPR = c(FPR, confusion.matrix[1,2])
r = r + 0.05
}

# ROC, Y = TPR, X = FPR
plot(FPR, TPR)




