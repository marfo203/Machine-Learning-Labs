data = read.csv("tecator.csv")

# Divide data randomly into train and test (50/50) by using the codes from the
# lectures

set.seed(12345)
id = sample(1:n, floor(n*0.5))
train = data[id, ]
test = data[-id, ]
