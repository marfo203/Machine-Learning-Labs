# Assignment 1 - Kernel Methods

# Lecture 3a, Kernel classification.

set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", header = TRUE, fileEncoding = "latin1")
temps <- read.csv("temps50k.csv", header = TRUE)
st <- merge(stations,temps,by="station_number")

# Kernel weight factors
h_distance <- 100000 # These three values are up to the students
h_date <- 15 # These three values are up to the students
h_time <- 10 # These three values are up to the students

# The data we want to make predictions on.
a <- 58.4274 # The point to predict (up to the students)
b <- 14.826 # The point to predict (up to the students)
point <- c(a,b)
date <- "2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", 
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp <- vector(length=length(times))
# Students’ code here
plot(temp, type="o")

# Remove posterior data
rm.posterior.data = function() {
  # if st$date == data --> remove
  current.data <- st[as.Date(st$date) < as.Date(date), ]
  return(current.data)
}

# Gaussian Kernel = k(u) = exp(-||u||^2), u = (x* - x) / h

# Kernel 1
# The first to account for the physical distance from a station to the point of interest.
# For this purpose, use the function distHaversine from the R package geosphere.
?distHaversine
distance.kernel = function(h) {
  current.data <- rm.posterior.data()
  distance <- distHaversine(point, as.matrix(select(current.data, longitude, latitude)))
  u <- distance / h
  return(exp(-u^2))
}
test = distance.kernel(100000)
# We want values close to 1 for close points and values close to 0 for distant points.
plot(distance <- distHaversine(point, as.matrix(select(current.data, longitude, latitude))), test)

# Kernel 2
# The second to account for the distance between the day a temperature measurement
# was made and the day of interest.
date.kernel = function(h) {
  current.data <- rm.posterior.data()
  number.of.days <- function (x) {
    return(difftime(as.Date(date), as.Date(x), units = "days"))
  }  
  days <- unlist(lapply(current.data$date, FUN = number.of.days))
  u <- days / h
  return(exp(-u^2))
}
test = date.kernel(15) # I Set it to 15 as it checks back approximately 30 days. 
# I don't know if that is enough.

# We want values close to 1 for close points and values close to 0 for distant points.
plot(unlist(lapply(current.data$date, FUN = number.of.days)), test, xlim = c(0, 50))

# Kernel 3
# The third to account for the distance between the hour of the day a 
# temperature measurement was made and the hour of interest.
time.kernel = function(time.of.interest, h) {
  current.data <- rm.posterior.data()
  time <- as.numeric(difftime(as.POSIXct(current.data$time, format = "%H:%M:%S"), 
                              as.POSIXct(time.of.interest, format = "%H:%M:%S"), units = "hours"))
  u <- time / h
  return(exp(-u^2))
}
time.of.interest <- "04:00:00"
test = time.kernel(time.of.interest, 10)
# We want values close to 1 for close points and values close to 0 for distant points.
plot(time <- as.numeric(difftime(as.POSIXct(current.data$time, format = "%H:%M:%S"),
                                 as.POSIXct(time.of.interest, format = "%H:%M:%S"), units = "hours")), test, xlim = c(-24, 24))

kernel.sum.prediction <- c()
kernel.sum.prediction <- NULL
kernel.product.prediction <- c()
kernel.product.prediction <- NULL
for (time in times) {
  # Summing Kernels
  kernel.sum <- distance.kernel(h_distance) + date.kernel(h_date) + time.kernel(time, h_time)
  kernel.sum.result <- sum(kernel.sum * current.data$air_temperature)/sum(kernel.sum)
  print("SUM")
  print(kernel.sum.result)
  kernel.sum.prediction <- c(kernel.sum.prediction, c(kernel.sum.result))
  # Multiplying Kernels
  kernel.product <- distance.kernel(h_distance) * date.kernel(h_date) * time.kernel(time, h_time)
  print(kernel.product)
  kernel.product.result <- sum(kernel.product * as.numeric(current.data$air_temperature))/sum(kernel.product)
  print("Prod")
  print(kernel.product.result)
  kernel.product.prediction <- c(kernel.product.prediction, c(kernel.product.result))
}
kernel.sum.prediction
kernel.product.prediction
plot(kernel.sum.prediction)
plot(kernel.product.prediction)
current.data$air_temperature
sum(1 * current.data$air_temperature)

