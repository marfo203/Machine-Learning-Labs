# Assignment 1 - Kernel Methods

# Lecture 3a, Kernel classification.

set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv", header = TRUE, fileEncoding = "latin1")
?read.csv()
temps <- read.csv("temps50k.csv", header = TRUE)
st <- merge(stations,temps,by="station_number")
h_distance <- 1 # These three values are up to the students
h_date <- 1 # These three values are up to the students
h_time <- 1 # These three values are up to the students
a <- 58.4274 # The point to predict (up to the students)
b <- 14.826 # The point to predict (up to the students)
date <- "2013-11-04" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00", "12:00:00", "14:00:00", 
           "16:00:00", "18:00:00", "20:00:00", "22:00:00", "24:00:00")
temp <- vector(length=length(times))
# Students’ code here
plot(temp, type="o")


# Gaussian Kernel = k(u) = exp(-||u||^2), u = (x* - x) / h

# Kernel 1
# The first to account for the physical distance from a station to the point of interest.
# For this purpose, use the function distHaversine from the R package geosphere.
?distHaversine

# Kernel 2
# The second to account for the distance between the day a temperature measurement
# was made and the day of interest.

# Kernel 3
# The third to account for the distance between the hour of the day a 
# temperature measurement was made and the hour of interest.

# Summing Kernels

# Multiplying Kernels
