library(caret)
library(ggplot2)

# 1. Divide it into training and test data (60/40) and scale it appropriately. In the
# coming steps, assume that motor_UPDRS is normally distributed and is a
# function of the voice characteristics, and since the data are scaled, no
# intercept is needed in the modelling.

data = read.csv("parkinsons.csv") # Reading the input data
data_frame = data.frame(data[c(5,7:22)]) # Stripping data of columns subject, age, sex, test_time, total_UPDRS
hist(data_frame$motor_UPDRS) # Checking the data distribution
plot(density(data_frame$motor_UPDRS)) # Checking the data distribution

#str(data)
#data_frame

set.seed(12345) # Setting the seed so that we get the same answers as other groups.
n = nrow(data_frame) # Extracting the number of rows in data.
trainId = sample(1:n, floor(n*0.6)) # Dividing the 60% into training data. 
train = data_frame[trainId, ] # Assigning the training data.
test = data_frame[-trainId, ] # Assigning the test data.

# Scaling the data
scaler=preProcess(train)
trainS=predict(scaler,train)
testS=predict(scaler,test)
data_frame
hist(trainS$motor_UPDRS)

# 2. Compute a linear regression model from the training data, estimate training
# and test MSE and comment on which variables contribute significantly to the model.

# Compute linear regression
fit=lm(motor_UPDRS~., data=trainS)
summary(fit)
#plot(fit)

# Histogram data follows noraml distr.
hist(fit$residuals, main='Histogram of residuals', ylab='residuals')

# Estimate training Mean Squared Error(MSE)
train_MSE = mean(((predict(fit, trainS)) - trainS$motor_UPDRS)^2)
print(paste("MSE train: ", train_MSE))

# Estimate test MSE
test_MSE = mean(((predict(fit, testS)) - testS$motor_UPDRS)^2)
print(paste("MSE test: ", test_MSE))


# Which values contribute significantly to the model
summary = summary(fit)
coef = as.data.frame(summary$coefficients)
coef$Estimate
sorted_coef = coef[order(-abs(coef$Estimate)),]
sorted_coef

X = as.matrix(trainS[,-1])
Y = as.matrix(trainS[,1])
n = nrow(trainS)
n_col = ncol(trainS)

# 3. Implement 4 following functions by using basic R commands only (noexternal packages):

# Loglikelihood
loglikelihood = function(theta, sigma) {
  sigmaSquared = sigma^2
  e = Y - X %*% theta
  loglik = 0.5 * n * log(2 * pi) - 0.5*n*log(sigmaSquared) - ((t(e) %*% e) / (2 * sigmaSquared))
  return(loglik)
}

# Ridge
ridge = function(param, lambda) {
  theta=param[1:16]
  sigma=param[17]
  ridge = (lambda*sum(theta^2) - loglikelihood(theta, sigma))
  return(ridge)
}

# RidgeOpt
# function that depends on scalar 𝜆, uses function from 3b and function optim() 
# with method=”BFGS” to find the optimal 𝜽 and 𝜎 for the given 𝜆.
ridgeOpt = function(lambda) {
  ridge_opt = optim(rep(1, 17), fn=ridge, lambda=lambda, method="BFGS")
  return(ridge_opt)
}
# DF
# function that for a given scalar 𝜆 computes the degrees of freedom 
# of the Ridge model based on the training data.
# Formula: X(X^t*X + lambda*I)^-1 * X^t
df = function(lambda) {
  Z = as.matrix(diag(lambda, 16, 16))
  # print(Z)
  df = X %*% ((t(X)%*%X) + Z)^-1 %*% t(X)
  return(tr(df))
}

# 4 By using function RidgeOpt, compute optimal 𝜽 parameters for 
#𝜆=1, 𝜆=100 and 𝜆=1 000.

ridge1 = ridgeOpt(1)
ridge1$par[1:16] #theta
ridge1$par[17] #sigma
ridge100 = ridgeOpt(100)
ridge1000 = ridgeOpt(1000)


# Predict motor_UPDRS
XTrain = as.matrix(trainS[,-1])
YTrain = as.matrix(trainS[,1])
XTest = as.matrix(testS[,-1])
YTest = as.matrix(testS[,1])

trainPred1 = XTrain %*% ridge1$par[1:16]
testPred1 = XTest %*% ridge1$par[1:16] 
trainPred100 = XTrain %*% ridge100$par[1:16]
testPred100 = XTest %*% ridge100$par[1:16]
trainPred1000 = XTrain %*% ridge1000$par[1:16]
testPred1000 = XTest %*% ridge1000$par[1:16]

mseTrain1 = mean((trainPred1 - Y)^2)
mseTest1 = mean((testPred1 - YTest)^2)
df(1)

mseTrain100 = mean((trainPred100 - Y)^2)
mseTest100 = mean((testPred100 - YTest)^2)     
df(100)

mseTrain1000 = mean((trainPred1000 - Y)^2)    
mseTest1000 = mean((testPred1000 - YTest)^2)
df(1000)
