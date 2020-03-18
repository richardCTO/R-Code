# set seed to keep results the same
set.seed(1234567)

####################### Problem 1 ###########################
# SRS of 400 out of 4000 with its mean
random_numbers <- c(1:4000)
sample_400 <- sample(random_numbers, 400, replace = FALSE)
mean(sample_400)

# SRS of 30 out of 300 with its mean
random_numbers <- c(1:300)
sample_30 <- sample(random_numbers, 30, replace = FALSE)
mean(sample_30)

# SRS of 3000 out of 300000000 with its mean
random_numbers <- c(1:300000000)
sample_3000 <- sample(random_numbers, 3000, replace = FALSE)
mean(sample_3000)

####################### Problem 3 ###########################
farm_data <- read.csv("STA4222/Data/agsrs.csv",
  header = TRUE
)
head(farm_data)

######## Part A ########
# mean
mean_87 <- mean(farm_data$acres87)
mean_87
# Standard deviation
acres87_sd <- sd(farm_data$acres87)
acres87_sd

# histogram of farm acres in 1987
hist(farm_data$acres87,
  main = "Farm Acres in 1987",
  xlab = "Farms",
  col = "navy",
  freq = TRUE
)

# confidence interval for acres in 1987
confidence_interval <- c(
    mean_87 - 2 * acres87_sd,
    mean_87 + 2 * acres87_sd
)
confidence_interval

######## Part B ########
# mean
mean_farms92 <- mean(farm_data$farms92)
mean_farms92
# Standard deviation
sd_farms92 <- sd(farm_data$farms92)
sd_farms92

# histogram
hist(farm_data$farms92,
  main = "Mean farms in 1992",
  xlab = "Farms",
  col = "darkgreen",
  freq = TRUE
)

# confidence interval
ci_mean_farms92 <- c(
    mean_farms92 - 2 * sd_farms92,
    mean_farms92 + 2 * sd_farms92
)
ci_mean_farms92

######## Part C ########
farm_data <- read.csv("STA4222/Data/agsrs.csv",
  header = TRUE
)

# To estimate the proportion of farms greater than 1000 acres
farms92_greater_than_1000 <- as.numeric(farm_data$acres92 > 1000)

# mean
mean_greater_1000 <- mean(farms92_greater_than_1000)
mean_greater_1000
# Standard deviation
sd_greater_1000 <- sd(farms92_greater_than_1000)
sd_greater_1000

# histogram
hist(farms92_greater_than_1000,
  main = "Mean farms greater than 1000 Acres in 1992",
  xlab = "Farms",
  col = "lightblue",
  freq = TRUE
)

# confidence interval
ci_farms_greater_than_1000 <- c(
    mean_greater_1000 - 2 * sd_greater_1000,
    mean_greater_1000 + 2 * sd_greater_1000
)
ci_farms_greater_than_1000

######## Part D ########
farm_data <- read.csv("STA4222/Data/agsrs.csv",
  header = TRUE
)

# To estimate the proportion of farms less than 9 acres
farms92_less_9 <- as.numeric(farm_data$acres92 <= 9)
farms92_less_9
# mean
mean_less_9 <- mean(farms92_less_9)
mean_less_9
# Standard deviation
sd_less_9 <- sd(farms92_less_9)
sd_less_9

# histogram
hist(farms92_less_9,
  main = "Mean farms Less than 9 Acres in 1992",
  xlab = "Farms",
  col = "darkmagenta",
  freq = TRUE
)

# confidence interval
ci_farms_less_9 <- c(
    mean_less_9 - 2 * sd_less_9,
    mean_less_9 + 2 * sd_less_9
)
ci_farms_less_9

####################### Problem 4 ###########################
# This wrong lol.. can't figure it out??

# z_val = 1.96, ...
# error = margin of error
# confidence = confidence level
# population = given population size
samp_size <- function(z_val, error, confidence, population) {
  ss <- (z_val^2 * error * (1 - error)) / (confidence^2)
  return(ss / (1 + ((ss - 1) / population)))
}
# Buckeye
samp_size(1.96, 0.04, 0.05, 4857)
# Gilbert

####################### Problem 5 ###########################
# This is not finished...
baseball_data <- read.csv("STA4222/Data/baseball.csv",
  header = FALSE
)
head(baseball_data)

set.seed(1234567)

baseball_n <- dim(baseball_data)[1]
baseball_n

n <- 300
srssam <- frame[sample(1:baseball_n, n, replace = FALSE),]
head(srssam)

baseball_sample <- sample(baseball_n, 150, replace = FALSE)
head(baseball_sample)

baseball_p <- as.numeric(baseball_data$V5)
baseball_p

