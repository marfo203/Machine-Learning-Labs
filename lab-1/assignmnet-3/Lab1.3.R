#R-script for assignment 3
install.packages("kknn")
install.packages("dplyr")
install.packages("Rfast")
install.packages("PerformanceAnalytics")
install.packages("psych")

install.packages("FSA")

library(dplyr)
library(FSA)
library(PerformanceAnalytics)
library(psych)
set.seed(12345)



data = read.csv("pima-indians-diabetes.csv", header=FALSE)
data

#Part 1: Scatter plot for plasma glucose concentration on age. Each scatter colored on Diabetes level

PlasmaGlucose = data[,2]
Age = data[,8]

plot(Age, PlasmaGlucose, col=as.factor(data[,9]), main = "Scatterplot of dataframe")
legend(x = "topright", legend=c("Diabetes", "No Diabetes"), col=c("red", "black"), pch=c(1))


#Part 2: Train Logistic regression model, Diabetes target var, plasma and age
#as features. Make prediction using r = 0,5 as classification thresh



set.seed(12345)
n=nrow(data)
id = sample(1:n, floor(n*0.7))
train = data[id, ]
test = data[-id,]



#Creating model with glm (generalized linear model)
model=glm(as.factor(data[,9]) ~ data[,8] + data[,2], data = data, family="binomial")
Prob = predict(model, data, type="response")

#threshholv 0.5 and factors 0 and 1
Pred = sapply(Prob, function(x) ifelse(x<0.5, 0 , 1))

confusion.matrix = table(data[,9], Pred)
confusion.matrix

#Missclassification
print(1-sum(diag(confusion.matrix))/sum(confusion.matrix))

summary(model)

#probabilittic model (P(diabetes = true) = 1 / 1 +  exp^(-5.91244 + 0.02477 * X8 + 0.035644 * X2)) given from summary 


#Plotting the model predictions


PlasmaGlucose = data[,2]
Age = data[,8]

plot(Age, PlasmaGlucose, col=as.factor(Pred), main="Scatterplot of prediction") #In the plot the red ones are the true diabetes, not the model
#To get scappet plot with colors on prediciton, remove train[,9] and apply only Pred

legend(x = "topright", legend=c("Diabetes", "No Diabetes"), col=c("red", "black"), pch=c(1))




#3: Use step 2 to calculate equation of the decition boundary between the two classes.
# theta0 + theta1 x1 + theta2 x2 = 0 -->

k = -coef(model)[2]/(coef(model)[3])
m= -coef( model)[1] / (coef(model)[3])



lines(abline(m, k))
# plasma = 162.589 - 0.597747age



# 4: plotting with r = 0,2 and r = 0,8

# r = 0,2
model=glm(as.factor(data[,9]) ~ data[,8] + data[,2], data = data, family="binomial")
Prob = predict(model, data, type="response")

summary(model)

r1 = 0.2
r2 = 0.8

#threshholv 0.2 and factors 0 and 1
Pred1 = sapply(Prob, function(x) ifelse(x<r1, 0 , 1))
Pred2 = sapply(Prob, function(x) ifelse(x<r2, 0 , 1))

summary(pred1)

confusion.matrix1 = table(data[,9], Pred1)
confusion.matrix2 = table(data[,9], Pred2)

#Missclassification
cat("Missclass, r = 0.2: ", 1- sum( diag(confusion.matrix1))/sum(confusion.matrix1))
cat("Missclass, r = 0.8: ", 1- sum( diag(confusion.matrix2))/sum(confusion.matrix2))

summary(model)

#probabilittic model (P(diabetes = true) = 1 / 1 +  exp^(-5.959 + 0.0210X50 + 0.03665X148)) given from summary 


#Plotting the model predictions


PlasmaGlucose = data[,2]
Age = data[,8]


plot(Age, PlasmaGlucose, col=as.factor(Pred1), main = "Scatterplot with r=0.2")
legend(x = "topright", legend=c("Diabetes", "No Diabetes"), col=c("red", "black"), pch=c(1))

plot(Age, PlasmaGlucose, col=as.factor(Pred2), main = "Scatterplot with r=0.8") 
legend(x = "topright", legend=c("Diabetes", "No Diabetes"), col=c("red", "black"), pch=c(1))


# 5: Basis Function Expansion, add them to dataset and perform logistic regression


#plasma = -162.589 + 0.597747age from part 3

x1 = data[,2]
x2 = data[,8]


z1 = x1^4
z2 = (x1^3)*x2
z3 = (x1^2)*x2^2
z4 = x1*x2^3
z5 = x2^4

data$z1 = z1
data$z2 = z2
data$z3 = z3
data$z4 = z4
data$z5 = z5

model=glm(as.factor(data[,9]) ~ data$V2+ data$V8+ z1 + z2 + z3 + z4 + z5, data = data, family="binomial")
Prob = predict(model, data, type="response")

Pred = sapply(Prob, function(x) ifelse(x<0.5, 0 , 1))

PlasmaGlucose = data[,2]
Age = data[,8]


plot(Age, PlasmaGlucose, col=as.factor(Pred), main = "Scatterplot using basis expansion trick") #In the plot the red ones are the true diabetes, not the model
legend(x = "topright", legend=c("Diabetes", "No Diabetes"), col=c("red", "black"), pch=c(1))

confusion.matrix = table(data[,9], Pred)
cat("Classification for Basis function expanstion ", 1- sum(diag(confusion.matrix))/sum(confusion.matrix))

