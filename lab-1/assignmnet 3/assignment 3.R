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



data = read.csv("pima-indians-diabetes.csv")
data

#Part 1: Scatter plot for plasma glucose concentration on age. Each scatter colored on Diabetes level

PlasmaGlucose = data[,2]
Age = data[,8]

plot(Age, PlasmaGlucose, col=as.factor(data$X1))
legend(x = "topright", legend=c("Diabetes", "No Diabetes"), col=c("red", "black"), pch=c(1))


#Part 2: Train Logistic regression model, Diabetes target var, plasma and age
#as features. Make prediction using r = 0,5 as classification thresh


data.num = select(data,X6, X148, X72, X35,  X0, X33.6, X0.627, X50, X1)
data.num

#make varible columns numeric
data.num$X50 = as.numeric(data.num$X50)
data.num$X148 = as.numeric(data.num$X148)
data.num$X1 = as.numeric(data.num$X1)

set.seed(12345)
n=nrow(data)
id = sample(1:n, floor(n*0.7))
train = data[id, ]
test = data[-id,]





#Creating model with glm (generalized linear model)
model=glm(as.factor(train$X1) ~ train$X50 + train$X148, data = train, family="binomial")
Prob = predict(model, train, type="response")



#threshholv 0.5 and factors 0 and 1
Pred = sapply(Prob, function(x) ifelse(x<0.5, 0 , 1))

confusion.matrix = table(train$X1, Pred)
confusion.matrix

#Missclassification
print(sum(diag(confusion.matrix))/sum(confusion.matrix))

summary(model)

#probabilittic model (P(diabetes = true) = 1 / 1 +  exp^(-5.959 + 0.0210X50 + 0.03665X148)) given from summary 


#Plotting the model predictions


PlasmaGlucose = train[,2]
Age = train[,8]

plot(Age, PlasmaGlucose, col=as.factor(train$X1)) #In the plot the red ones are the true diabetes, not the model
#To get scappet plot with colors on prediciton, remove train$X1 and apply only Pred

legend(x = "topright", legend=c("Diabetes", "No Diabetes"), col=c("red", "black"), pch=c(1))




#3: Use step 2 to calculate equation of the decition boundary between the two classes.
# theta0 + theta1 x1 + theta2 x2 = 0 -->
coef(model)
head(train)
k = coef(model)[2]/(-coef(model)[3])
m=coef(model)[1] / (-coef(model)[3])

coef(model)[2]


lines(abline(m, k))
# plasma = -162.589 + 0.597747age



# 4: plotting with r = 0,2 and r = 0,8

# r = 0,2
model=glm(as.factor(train$X1) ~ train$X50 + train$X148, data = train, family="binomial")
Prob = predict(model, train, type="response")

r1 = 0.2
r2 = 0.8

#threshholv 0.2 and factors 0 and 1
Pred1 = sapply(Prob, function(x) ifelse(x<r1, 0 , 1))
Pred2 = sapply(Prob, function(x) ifelse(x<r2, 0 , 1))

confusion.matrix1 = table(train$X1, Pred1)
confusion.matrix2 = table(train$X1, Pred2)

#Missclassification
cat("Missclass, r = 0.2: ",  sum(diag(confusion.matrix1))/sum(confusion.matrix1))
cat("Missclass, r = 0.8: ",  sum(diag(confusion.matrix2))/sum(confusion.matrix2))

summary(model)

#probabilittic model (P(diabetes = true) = 1 / 1 +  exp^(-5.959 + 0.0210X50 + 0.03665X148)) given from summary 


#Plotting the model predictions


PlasmaGlucose = train[,2]
Age = train[,8]

plot(Age, PlasmaGlucose, col=as.factor(Pred)) #In the plot the red ones are the true diabetes, not the model
legend(x = "topright", legend=c("Diabetes", "No Diabetes"), col=c("red", "black"), pch=c(1))


model=glm(as.factor(train$X1) ~ train$X50 + train$X148, data = train, family="binomial")
Prob = predict(model, train, type="response")




