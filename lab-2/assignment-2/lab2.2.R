# install.packages("tree")

# 1. 
data = read.csv2("bank-full.csv") # We use read.csv2 since the delimitor is ";"
data = as.data.frame(data%>%select(-duration))
# Converting 'yes and 'character' no to factor
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

type <- sapply(data,class)
type
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

# Misclassification rate test
Yfit = predict(fit, newdata = test, type = "class")
missclass.matrix = table(test$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for training data: ", missclass.rate))

# b. Decision Tree with smallest allowed node size equal to 7000.
?tree
fit = tree(y~., data=train, control = tree.control(nrow(train), minsize = 7000))

# Misclassification rate training
Yfit = predict(fit, newdata = train, type = "class")
missclass.matrix = table(train$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for training data: ", missclass.rate))

# Misclassification rate test
Yfit = predict(fit, newdata = test, type = "class")
missclass.matrix = table(test$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for training data: ", missclass.rate))


# c. Decision trees minimum deviance to 0.0005.
fit = tree(y~., data=train, control = tree.control(nrow(train), mindev = 0.0005))

# Misclassification rate training
Yfit = predict(fit, newdata = train, type = "class")
missclass.matrix = table(train$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for training data: ", missclass.rate))

# Misclassification rate test
Yfit = predict(fit, newdata = test, type = "class")
missclass.matrix = table(test$y, Yfit)
missclass.rate = 1 - sum(diag(missclass.matrix)) / sum(missclass.matrix)
print(paste("Missclassification rate for training data: ", missclass.rate))

# 3. 

# 4. 

# 5. 

# 6. 









