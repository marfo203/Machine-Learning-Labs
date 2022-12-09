# Assignment 2

# Lab 3 block 1 of 732A99/TDDE01/732A68 Machine Learning
# Author: jose.m.pena@liu.se
# Made for teaching purposes
# install.packages("kernlab")
library(kernlab)
set.seed(1234567890)

data(spam)
foo <- sample(nrow(spam))
spam <- spam[foo,]
spam[,-58]<-scale(spam[,-58])
tr <- spam[1:3000, ]
va <- spam[3001:3800, ]
trva <- spam[1:3800, ]
te <- spam[3801:4601, ] 

by <- 0.3
err_va <- NULL
c <- NULL
for(i in seq(by,5,by)){
  filter <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=i,scaled=FALSE)
  mailtype <- predict(filter,va[,-58])
  t <- table(mailtype,va[,58])
  err_va <-c(err_va,(t[1,2]+t[2,1])/sum(t))
  c = c(c, i)
}
err_va
c
which.min(err_va)
c[13]
which.min(err_va)*by


filter0 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter0,va[,-58])
t <- table(mailtype,va[,58])
err0 <- (t[1,2]+t[2,1])/sum(t)
err0
t

filter1 <- ksvm(type~.,data=tr,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter1,te[,-58])
t <- table(mailtype,te[,58])
err1 <- (t[1,2]+t[2,1])/sum(t)
err1
t

filter2 <- ksvm(type~.,data=trva,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter2,te[,-58])
t <- table(mailtype,te[,58])
err2 <- (t[1,2]+t[2,1])/sum(t)
err2
t

filter3 <- ksvm(type~.,data=spam,kernel="rbfdot",kpar=list(sigma=0.05),C=which.min(err_va)*by,scaled=FALSE)
mailtype <- predict(filter3,te[,-58])
t <- table(mailtype,te[,58])
err3 <- (t[1,2]+t[2,1])/sum(t)
err3
t
# Questions

# 1. Which filter do we return to the user ? filter0, filter1, filter2 or filter3? Why?
# All models use the optimal C value calculated in the for loop.
# filter0 | Uses the training set for training, testing on the validation set with error 0.0675
# filter1 | Uses the training set for training, testing on the test set with error 0.08489388
# filter2 | Uses the training and validation set for training, testing on the test set with error 0.082397
# filter3 | Uses whole data set for training, testing on the test set with error 0.02122347

# I think that filter 0 or filter 1 with the lowest error rate or filter 2 which is based on more training data are good candidates. I would consider filter 2 as a better 
# candidate as it is fitted with more training data and should therefore be able to generalize better on new unseen data. 

# Since we are calculating the optimal C value with the training data tr we should use a model that is trained with the same data.
# Therefore we should use filter 0 or 1 since they are the same. 

# 2. What is the estimate of the generalization error of the filter returned to the user? err0, err1, err2 or err3? Why?

# err0 -> Error when training on the training set and predicting on the validation set. 0.0675
# err1 -> Error when training on the training set and predicting on the test set. This seems worse than filter 0 but it is the same model tested on a new data set. 0.08489388
# err2 -> Error when training on the training + validation set and predicting on the test set. It is slightly better than filter 0/1, probably because the model is trained on more data. 0.082397
# err3 -> Error when training on the whole data set and predicting on the test set. The error is quite low since we train the model on the whole data set and then use that data to test the model. 0.02122347

# 3. Implementation of SVM predictions.
sv<-alphaindex(filter3)[[1]]
co<-coef(filter3)[[1]]
co
inte<- - b(filter3)
rbf <- rbfdot(sigma = 0.05) # Used to produce the kernel function with the same sigma as above.
k<-NULL
for(i in 1:10) { # We produce predictions for just the first 10 points in the dataset.
  k2<-0 # NULL does not work when doing the addition below. Then it returns a numeric(0) all of the time.
  for(j in 1:length(sv)) {
    k2 <- k2 + co[j] * rbf(unlist(spam[sv[j], -58]) # The training data
                           , unlist(spam[i, -58] # The test data that we want to predict on.
                                    )) # alpha * kernel for each element.
  }
  k<-c(k, k2 + inte) # Adding the intercept to the prediction. 
}
k # The sign on k can be used for classification. Positive values are spam and negative nonspam.

predict(filter3,spam[1:10,-58], type = "decision")


