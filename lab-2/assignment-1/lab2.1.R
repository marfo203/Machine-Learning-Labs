data = read.csv("tecator.csv")

?read.csv
# Divide data randomly into train and test (50/50) by using the codes from the
# lectures

set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = data[id, ]
test = data[-id, ]

# 1. Assume that Fat can be modeled as a linear regression in which absorbance
# characteristics (Channels) are used as features. Report the underlying
# probabilistic model, fit the linear regression to the training data and estimate
# the training and test errors. Comment on the quality of fit and prediction and
# therefore on the quality of model.


# 2.

# 3.

# 4.

# 5.
