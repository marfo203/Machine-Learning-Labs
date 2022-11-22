# install.packages("tree")

# 1. 
data = read.csv2("bank-full.csv") # We use read.csv2 since the delimitor is ";"
data = as.data.frame(data%>%select(-duration))

# Converting yes to 1 and no to 0 in y column.
data$y = as.factor(data$y)

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

# Misclassification rate training
Yfit = predict(fit, train)

# Misclassification rate test
Yfit = predict(fit, newdata = test)

# b. Decision Tree with smallest allowed node size equal to 7000.

# Misclassification rate training
Yfit = predict(fit, newdata = train)

# Misclassification rate test
Yfit = predict(fit, newdata = test)

# c. Decision trees minimum deviance to 0.0005.

# Misclassification rate training
Yfit = predict(fit, newdata = train)

# Misclassification rate test
Yfit = predict(fit, newdata = test)

# 3. 

# 4. 

# 5. 

# 6. 









