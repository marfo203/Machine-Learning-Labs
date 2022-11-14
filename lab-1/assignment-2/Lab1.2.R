library(caret)
library(ggplot2)

# 1. Divide it into training and test data (60/40) and scale it appropriately. In the
# coming steps, assume that motor_UPDRS is normally distributed and is a
# function of the voice characteristics, and since the data are scaled, no
# intercept is needed in the modelling.

data = read.csv("parkinsons.csv") # Reading the input data
data_frame = data.frame(data[c(5,7:22)])
hist(data_frame$motor_UPDRS) # Checking the data distribution
plot(density(data_frame$motor_UPDRS)) # Checking the data distribution
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
plot(fit)

# Estimate training Mean Squared Error(MSE)
train_MSE = mean((trainS$motor_UPDRS - predict.lm(fit, trainS))^2)
train_MSE

# Estimate test MSE
test_MSE =  mean((testS$motor_UPDRS - predict.lm(fit, testS))^2)
test_MSE

# Which values contribute significantly to the model
summary = summary(fit)
coef = as.data.frame(summary$coefficients)
coef$Estimate
sorted_coef = coef[order(-abs(coef$Estimate)),]
sorted_coef

# 3. Implement 4 following functions by using basic R commands only (noexternal packages):
X = trainS[,-1]
Y = trainS[,1]  
# Loglikelihood
loglikelihood = function(theta, sigma) {
  n = nrow(trainS)
  beta = theta[1]
  sigmaSquared = sigma^2
  e = Y - beta * X 
  loglik = 0.5 * n * log(2 * pi) - 0.5*n*log(sigmaSquared) - ((t(e) %*% e) / (2 * sigmaSquared))
  return(-loglik)
}

# Ridge
ridge = function(theta, sigma, lambda) {
  ridge = (lambda*sum(theta^2) - loglikelihood(theta, sigma))
  return(ridge)
}

# RidgeOpt
# function that depends on scalar 𝜆, uses function from 3b and function optim() 
# with method=”BFGS” to find the optimal 𝜽 and 𝜎 for the given 𝜆.
ridgeOpt = function(lambda) {
  ridge_opt = optim(X, fn=ridge, lambda=lambda, method="BFGS")
  return(ridge_opt)
}

# DF
# function that for a given scalar 𝜆 computes the degrees of freedom 
# of the Ridge model based on the training data.
# Formula: X(X^t*X + lambda*I)^-1 * X^t
I = diag(n)
df = function(lambda) {
  df = X %*% (t(X)%*%X + lambda*I)^-1 %*% t(X) 
  return(df)
}

# 4 By using function RidgeOpt, compute optimal 𝜽 parameters for 
# 𝜆 = 1, 𝜆 = 100 and 𝜆 = 1000.




