# Assignment 3
# 1.
# install.packages("neuralnet")
library(neuralnet)
set.seed(1234567890)

Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))
tr <- mydata[1:25,] # Training
te <- mydata[26:500,] # Test

# Random initialization of the weights in the interval [-1, 1]
set.seed(1234567890)
winit <- runif(31, min = -1, max = 1) # Plot the network and this is quite trivial to count. The number of nodes * 3 + 1. A input W1X + b1 (one for each xi) and then W2Q + b2.
nn <- neuralnet(Sin~., data = tr, hidden = c(10), startweights = winit)
    
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)
plot(nn)
  
# 2. 
    
# Linear
linear <- function (x) x

nn <- neuralnet(Sin~., data = tr, hidden = c(10), startweights = winit, act.fct = linear)
# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)
# Comment: A combination of only linear activation functions will result in a linear output. 
    
# ReLU
# install.packages("sigmoid")
library(sigmoid) # Used to import the relu function from the sigmoid package, which is differentiable.

nn <- neuralnet(Sin~., data = tr, hidden = c(10), startweights = winit, act.fct = relu)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

# Softplus
softplus <- function(x) log(1 + exp(x))
nn <- neuralnet(Sin~., data = tr, hidden = c(10), startweights = winit, act.fct = softplus)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(tr, cex=2)
points(te, col = "blue", cex=1)
points(te[,1],predict(nn,te), col="red", cex=1)

# 3. 
set.seed(1234567890) # Should it be set.seed(12345) ?

Var <- runif(500, 0, 50)
mydata <- data.frame(Var, Sin=sin(Var))

# Plot of the training data (black), test data (blue), and predictions (red)
plot(mydata[,1],predict(nn, mydata), col="red", cex=1)
points(mydata, cex=1)
points(mydata, col = "blue", cex=1)


# 4. 
nn
nn$weights
plot(nn)
abline(h = -7.47, lwd = 4) # Sum of all weights for nodes with positive inputs.

# 5. 
library(neuralnet)
set.seed(1234567890) # Should it be set.seed(12345) ?

Var <- runif(500, 0, 10)
mydata <- data.frame(Var, Sin=sin(Var))

# Random initialization of the weights in the interval [-1, 1]
set.seed(1234567890) # Should it be set.seed(12345) ?
winit <- runif(31, min = -1, max = 1)
nn <- neuralnet(Var~., data = mydata, hidden = c(10), startweights = winit, threshold = 0.1)

# Plot of the training data (black), test data (blue), and predictions (red)
plot(predict(nn, mydata), mydata[,2], col="red", cex=1)
points(mydata)
points(mydata, col = "blue", cex=1)
# Comment, it is expected that we get a bad result if we try to model the invers of a
# non-invertible function such as sin(x). 

