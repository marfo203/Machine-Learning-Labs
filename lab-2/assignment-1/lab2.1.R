install.packages("glmnet")
install.packages("ggplot2")

data = read.csv("tecator.csv")

# Divide data randomly into train and test (50/50) by using the codes from the
# lectures

set.seed(12345)
n = nrow(data)
id = sample(1:n, floor(n*0.5))
relData = data.frame(data[c(2:102)])
train = relData[id, ]
test = relData[-id, ]

# 1. Assume that Fat can be modeled as a linear regression in which absorbance
# characteristics (Channels) are used as features. Report the underlying
# probabilistic model, fit the linear regression to the training data and estimate
# the training and test errors. Comment on the quality of fit and prediction and
# therefore on the quality of model.
fit = lm(Fat~., data=train)
summary(fit)

# Estimate training Mean Squared Error(MSE)
train_MSE = mean((train$Fat - (predict(fit, train)))^2)
print(paste("MSE train: ", train_MSE))

# Estimate test MSE
test_MSE = mean((test$Fat - (predict(fit, test)))^2)
print(paste("MSE test: ", test_MSE))

# Bad quality!

# 2.
library(glmnet)
x = as.matrix(train%>%select(-Fat))
y = as.matrix(train%>%select(Fat))
lasso = glmnet(x, y, family="gaussian", lambda = 1, alpha = 1)
# Cost function squared error p.g.a. att vi kör gaussion.
coef(lasso)
# Linear regression model: Fat = -12 + 9,7 * Channel41
# Cost function: min theta = MSE + lambda * (12+9)

# 3.
plotVector = vector("numeric")
for (n in 101:260) {
  lasso = glmnet(x, y, family="gaussian", lambda = log(n/100), alpha = 1)
  plotVector = c(plotVector, lasso$df)
}
library(ggplot2)
plot(plotVector)
which(plotVector %in% c(3))

# 4.
plotVector = vector("numeric")
for (n in 1:1000) {
  ridge = glmnet(x, y, family="gaussian", lambda = log(1), alpha = 0)
  plotVector = c(plotVector, ridge$df)
}
library(ggplot2)
plot(plotVector)
which(plotVector %in% c(3))


# 5.
