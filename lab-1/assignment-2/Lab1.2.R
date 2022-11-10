library(caret)
library(ggplot2)

# 1. Divide it into training and test data (60/40) and scale it appropriately. In the
# coming steps, assume that motor_UPDRS is normally distributed and is a
# function of the voice characteristics, and since the data are scaled, no
# intercept is needed in the modelling.

data = read.csv("parkinsons.csv") # Reading the input data
set.seed(12345) # Setting the seed so that we get the same answers as other groups.
n = nrow(data) # Extracting the number of rows in data.
trainId = sample(1:n, floor(n*0.6)) # Dividing the 60% into training data. 
train = data[trainId, ] # Assigning the training data.
test = data[-trainId, ] # Assigning the test data.

# Scaling the data
scaler=preProcess(train)
trainS=predict(scaler,train)
testS=predict(scaler,test)

# 2. Compute a linear regression model from the training data, estimate training
# and test MSE and comment on which variables contribute significantly to the model.

# Compute linear regression
fit=lm(motor_UPDRS~., data=trainS)
summary(fit)

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

# Loglikelihood
loglikelihood = function(vector, dispersion) {
  
}

# Ridge


# RidgeOpt


# DF





