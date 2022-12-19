# Assignment 1 - Kernel Methods
#install.packages("dplyr")

set.seed(1234567890)
library(geosphere)
library(dplyr)
# Reading the data, needs file encoding bacause swedish names. Don't forget header!
stations <- read.csv("stations.csv", header = TRUE, fileEncoding = "latin1")
temps <- read.csv("temps50k.csv", header = TRUE)

# Merge data from both files to one
st <- merge(stations,temps,by="station_number")

# Kernel weight factors
h_distance <- 100000
h_date <- 10
h_time <- 5

# The data we want to make predictions on.
# The point to predict (up to the students)
a <- 58.4274
b <- 14.826

# Combining the points together
point <- c(b,a) # Longitude first and then latitude, since distHaversine() needs 
                # it in this order. Point is in Vadstena.

date <- "2011-08-20" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", 
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp <- vector(length=length(times))


############################ Students’ code here: ##############################

# Vectors to fill with temps when taking the sum/product of kernels
temp.sum <- vector(length=length(times))
temp.mult <- vector(length=length(times))

# Remove posterior data
rm.posterior.data = function() {
  current.data <- st[as.Date(st$date) < as.Date(date), ]
  return(current.data)
}
current.data <- rm.posterior.data()
# Percentage of data from data set that is used
no_curr <- nrow(current.data)
no_all <- nrow(st)
percent = no_curr / no_all


# Gaussian Kernel = k(u) = exp(-||u||^2), u = (x* - x) / h

############################## Kernel 1 - Distance #############################

# The first to account for the physical distance from a station to the point of interest.
# For this purpose, use the function distHaversine from the R package geosphere.
distance.kernel = function(h) {
  distance <<- distHaversine(data.frame(current.data$longitude, current.data$latitude), 
                             point)
  u <- distance / h
  return(exp(-u^2))
}

# Testing kernel h-values
# We want values close to 1 for close points and values close to 0 for distant points.
test = distance.kernel(100000)
plot(distance, test, ylab = "Kernel value", 
     xlab = "Distance", main="Distance kernel values as function of distance")

############################### Kernel 2 - Date ################################

# The second to account for the distance between the day a temperature measurement
# was made and the day of interest.
date.kernel = function(h) {
  number.of.days <<- function (x) {
    return(difftime(as.Date(date), as.Date(x), units = "days"))
  }  
  days <- unlist(lapply(current.data$date, FUN = number.of.days))
  u <- days / h
  return(exp(-u^2))
}

# Testing kernel h-values
# We want values close to 1 for close points and values close to 0 for distant points.
test = date.kernel(10)
plot(unlist(lapply(current.data$date, FUN = number.of.days)), test, xlim = c(0, 50), 
     ylab = "Kernel value", xlab = "Days", main="Date kernel values as function of date")

################################ Kernel 3 - Time################################

# The third to account for the distance between the hour of the day a 
# temperature measurement was made and the hour of interest.
time.kernel = function(time.of.interest, h) {
  time <- as.numeric(difftime(as.POSIXct(current.data$time, format = "%H:%M:%S"), 
                              as.POSIXct(time.of.interest, format = "%H:%M:%S"), 
                              units = "hours"))
  time[which(abs(time)>12)] = 24 - abs(time[which(abs(time)>12)])
  u <- time / h
  return(exp(-u^2))
}
# Testing kernel h-values
# We want values close to 1 for close points and values close to 0 for distant points.
time.of.interest <- "12:00:00"
test = time.kernel(time.of.interest, 5)
plot(time <- as.numeric(difftime(as.POSIXct(current.data$time, format = "%H:%M:%S"),
                                 as.POSIXct(time.of.interest, format = "%H:%M:%S"), 
                                 units = "hours")), test, xlim = c(-24, 24), 
                                 ylab = "Kernel value", xlab = "Hours", 
                                 main="Time kernel values as function of hours")


# "Pre-fretch/calculate data" to speed up calculations
data.dist <- distance.kernel(h_distance) # Calculate distance kernel
data.date <- date.kernel(h_date) # Calculate date kernel
# Create matrix to fill with values from time kernel
data.hour <- matrix(nrow = dim(current.data)[1], ncol = length(times))
i = 1
# Loop through times and calculate time kernel value for each. Save in matrix
for(time in times) {
  data.hour[,i] <- time.kernel(time, h_time)
  i = i + 1
}


for(i in 1:length(times)){
  data.sum <- (data.dist + data.date + data.hour[,i])
  pred.sum <- sum(data.sum * current.data$air_temperature)/sum(data.sum)
  temp.sum[i] <- pred.sum

  data.mult <- (data.dist * data.date * data.hour[,i])
  pred.mult <- sum(data.mult * (current.data$air_temperature))/sum(data.mult)
  temp.mult[i] <- pred.mult
}

# Create a vector of numerical values of time to plot against
times.plot <- c(4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24)

# Plot sum of kernels
plot(times.plot, temp.sum, type="o", ylab = "Temperature (C°)", 
     xlab = "Hours in day", main="Predicted temp, Sum of kernels")

# Plot product of kernels
plot(times.plot, temp.mult, type="o", ylab = "Temperature (C°)", 
     xlab = "Hours in day", main="Predicted temp, Product of kernels")

################## Checking data from selected date to compare #################
st[st$date == as.Date(date), ]
temps[temps$date == as.Date(date), ]

