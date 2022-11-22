
# 1. 
data = read.csv2("bank-full.csv") # We use read.csv2 since the delimitor is ";"
data = data%>%select(-duration)

library(dplyr)
set.seed(12345)
n=nrow(data)
train.id = sample(1:n, floor(n*0.4))
train = data[train.id, ]

temp.id = setdiff(1:n, train.id)
set.seed(12345)
valid.id = sample(temp.id, floor(n*0.3))
valid = data[valid.id, ]

test.id = setdiff(temp.id, valid.id)
test = data[test.id, ]

# 2. 