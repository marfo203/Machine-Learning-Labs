# ---------------------------------------------- ASSIGNMENT 3 ----------------------------------------------------

# ------------------------------------------- Setup and read data ------------------------------------------------
library(caret)
library(ggplot2)

data <- read.csv("communities.csv")
#View(data)


# ------------------------------------------- TASK 1 ------------------------------------------------------------
# 1. Scale all variables except of ViolentCrimesPerPop and implement PCA by using function eigen().

# Scaling the data
data_frame <- data.frame(data[c(1:100)]) # Stripping data of column ViolentCrimesPerPop
scaler <- preProcess(data_frame)
dataS <- predict(scaler,data_frame)

# Check mean=0
summary(dataS)

cov_matrix = cov(dataS)
eigen_values = eigen(cov_matrix)

# ------------------------------------------- TASK 2 ------------------------------------------------------------
# 2. Repeat PCA analysis by using princomp() function and make the trace plot of the first principle component.
# Also provide a plot of the PC scores in the coordinates (PC1, PC2) in which the color of the points is given by
# ViolentCrimesPerPop. Analyse this plot (hint: use ggplot2 package ).


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

