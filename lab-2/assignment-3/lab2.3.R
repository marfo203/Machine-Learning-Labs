# ---------------------------------------------- ASSIGNMENT 3 ----------------------------------------------------

# ------------------------------------------- Setup and read data ------------------------------------------------
install.packages("ggfortify")

library(caret)
library(ggplot2)
set.seed(12345)

data <- read.csv("communities.csv")
#View(data)


# ------------------------------------------- TASK 1 ------------------------------------------------------------
# 1. Scale all variables except of ViolentCrimesPerPop and implement PCA by using function eigen().

# Scaling the data
data_frame <- data.frame(data[c(1:100)]) # Stripping data of column ViolentCrimesPerPop
scaler <- preProcess(data_frame)
dataS <- predict(scaler,data_frame)

data_frame2 <- data.frame(data) 
scaler2 <- preProcess(data_frame2)
dataS2 <- predict(scaler,data_frame2)


# Check mean=0
summary(dataS)

cov_matrix = cov(dataS)
eigen_values = eigen(cov_matrix)

summary(eigen_values)

lambda= eigen_values$values #lambda är lika med varieansen för en komponent

eigen_values
lambda

var = sprintf("%2.3f", lambda/sum(lambda)*100) #In we add component 
var

calcVar <- function (var) {
  sumVar = 0
  for (i in 1:100){
    sumVar = sumVar + as.numeric(var[i])
    print(sumVar)
    if (sumVar>=95){
      index = i
      print(index)
      break
    }
  }
}


calcVar(var) #calculating hom many comonents are needed for 95%


res=prcomp(dataS)
?prcomp
screeplot(res)
plot(res)


#What is the proportion of variation
#explained by each of the first two principal components? 
#Tolkar detta som 25+16



# ------------------------------------------- TASK 2 ------------------------------------------------------------
# 2. Repeat PCA analysis by using princomp() function and make the trace plot of the first principle component.
# Also provide a plot of the PC scores in the coordinates (PC1, PC2) in which the color of the points is given by
# ViolentCrimesPerPop. Analyse this plot (hint: use ggplot2 package ).

prin.comp <- princomp(dataS)

prin.comp
?princomp
#in princomp loadings is used instead of rotations used in lecture
U <- prin.comp$loadings
U
U[,1]

#plotting traceplot
plot(sort(abs(U[,1]), decreasing = T))

contribute.U <- head(sort(abs(U[,1]), decreasing = T, n = 5))
contribute.U
#plot of pc scored, colors given by violentCrimesPerPop
library(ggplot2)
library(ggfortify)

autoplot(prin.comp, data=dataS2, colour='ViolentCrimesPerPop')

# ------------------------------------------- TASK 3 ------------------------------------------------------------
# 3. Split the original data into training and test (50/50) and scale both features and response appropriately, 
# and estimate a linear regression model from training data in which ViolentCrimesPerPop is target and all other
# data columns are features. Compute training and test errors for these data and comment on the quality of model.

# Reading the input data
data <- read.csv("communities.csv")

# Setting the seed so that we get the same answers as other groups.
set.seed(12345) 

# Extracting the number of rows in data.
n <- nrow(data)
# Dividing the 50% into training data.
trainId = sample(1:n, floor(n*0.5))
# Assigning the training data.
train = data[trainId, ]
# Assigning the test data.
test = data[-trainId, ]

# Scaling the data
scaler=preProcess(train)
trainS=predict(scaler,train)
testS=predict(scaler,test)

fit=lm(ViolentCrimesPerPop~., data=trainS)
summary(fit)

# Estimate training Mean Squared Error(MSE)
train_MSE = mean((trainS$ViolentCrimesPerPop - (predict(fit, trainS)))^2)
print(paste("MSE train: ", train_MSE))

# Estimate test MSE
test_MSE = mean((testS$ViolentCrimesPerPop - (predict(fit, testS)))^2)
print(paste("MSE test: ", test_MSE))


# ------------------------------------------- TASK 4 ------------------------------------------------------------
# 4. Implement a function that depends on parameter vector 𝜃 and represents the cost function for 
# linear regression without intercept on the training data set.

train.x <- as.matrix(trainS[,-101])
train.y <- as.matrix(trainS[,101])

test.x <- as.matrix(testS[,-101])
test.y <- as.matrix(testS[,101])



train.vector <- vector("numeric")
test.vector <- vector("numeric") 

costLinReg <- function(theta){
  
  train.cost = sum( ((train.x%*%theta) - train.y )^2)/(nrow(train.x))
  test.cost = sum( ((test.x%*%theta) - test.y )^2)/(nrow(test.x))
  
  test.vector<<- c(test.vector, test.cost)
  
  train.vector<<- c(train.vector, train.cost)
  
  return (train.cost);
}

print(test.vector)

test.cost

theta.0 <- as.matrix(rep(0,100))

theta.opt <- optim(theta.0, fn=costLinReg, method="BFGS")


plot(test.vector[-(0:500)], ylim=c(0,1), xlim=c(-5,5000))
points(train.vector[-(0:500)], col="blue")
abline(v=1682)

which.min(test.vector)

test.vector[2182]
train.vector[2182]





theta.opt$par



