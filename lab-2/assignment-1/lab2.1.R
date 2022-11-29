install.packages("glmnet")
install.packages("ggplot2")
install.packages("dplyr")

data = read.csv("tecator.csv")
library(dplyr)

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
lasso = glmnet(x, y, family="gaussian", alpha = 1)

#lasso = cv.glmnet(x, y, family="gaussian", alpha = 1)
plot(lasso)

# Cost function squared error p.g.a. att vi kör gaussion.

coef(lasso)
# Linear regression model: Fat = -12 + 9,7 * Channel41
# Cost function: min theta = MSE + lambda * (12+9)

# 3.
library(caret)
x = as.matrix(train%>%select(-Fat))
y = as.matrix(train%>%select(Fat))
lasso = glmnet(x, y, family="gaussian", alpha = 1)


plot(lasso, xvar="lambda", label=TRUE, xlim=c(-0.345,-0.09))

print(lasso)

# 4.
x = as.matrix(train%>%select(-Fat))
y = as.matrix(train%>%select(Fat))
lasso = glmnet(x, y, family="gaussian", alpha = 0)


plot(lasso, xvar="lambda", label=TRUE, xlim=c(8,10), ylim=c(-0.05,0.05))

print(lasso)


# 5.
 #först här ska cv användas
lassoCV = cv.glmnet(x, y, family="gaussian", alpha=1)
plot(lassoCV)
coef(lassoCV, s="lambda.min")

log(lassoCV$lambda.min) #omptimal lambda
nice = lassoCV$lambda.min

lasso = glmnet(x, y, family="gaussian", alpha = 1, lambda = nice)

newx = as.matrix(test%>%select(-Fat))
newy = as.matrix(test%>%select(Fat))

plot(predict(lasso, newx), col = "green")
points(newy, col = "red")




