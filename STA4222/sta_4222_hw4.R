# ==========================================
#               Problem 2
# ==========================================

spanish_data <- read.csv("STA4222/Data/spanish.csv")
head(spanish_data)

# ================= Part A ===================
population_size <- 72
sample_size <- 10

goining_on_trip <- sum(spanish_data$trip == 1)
goining_on_trip

##  2a estimate the total number of students planing a trip
t_hat <- population_size / sample_size * goining_on_trip
t_hat

t_bar <- t_hat / population_size
t_bar

unique(spanish_data$class)

m1 <- sum(spanish_data[which(spanish_data$class == 34 & spanish_data$trip == 1), 3])
m2 <- sum(spanish_data[which(spanish_data$class == 65 & spanish_data$trip == 1), 3])
m3 <- sum(spanish_data[which(spanish_data$class == 60 & spanish_data$trip == 1), 3])
m4 <- sum(spanish_data[which(spanish_data$class == 69 & spanish_data$trip == 1), 3])
m5 <- sum(spanish_data[which(spanish_data$class == 68 & spanish_data$trip == 1), 3])
m6 <- sum(spanish_data[which(spanish_data$class == 20 & spanish_data$trip == 1), 3])
m7 <- sum(spanish_data[which(spanish_data$class == 47 & spanish_data$trip == 1), 3])
m8 <- sum(spanish_data[which(spanish_data$class == 23 & spanish_data$trip == 1), 3])
m9 <- sum(spanish_data[which(spanish_data$class == 4 & spanish_data$trip == 1), 3])
m10 <- sum(spanish_data[which(spanish_data$class == 39 & spanish_data$trip == 1), 3])

total <- sum(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)
total

n <- 10
##  calculate s^2
s_squard <- ((1 / (n - 1)) *
    (((m1 - t_bar)^2) + ((m2 - t_bar)^2) +
        ((m3 - t_bar)^2) + ((m4 - t_bar)^2) +
        ((m5 - t_bar)^2) + ((m6 - t_bar)^2) +
        ((m7 - t_bar)^2) + ((m8 - t_bar)^2) +
        ((m9 - t_bar)^2) + ((m10 - t_bar)^2))
)
s_squard
# standard error
error <- population_size * sqrt((1 - n / population_size) * (s_squard / n))
error
##  2a confidence interval 
ci_1 <- (t_hat - 1.96 * (error))
ci_2 <- (t_hat + 1.96 * (error))
ci <- c(ci_1, ci_2)
ci

# ================= Part B ===================
m1 <- length(spanish_data[which(spanish_data$class == 34), 2])
m2 <- length(spanish_data[which(spanish_data$class == 65), 2])
m3 <- length(spanish_data[which(spanish_data$class == 60), 2])
m4 <- length(spanish_data[which(spanish_data$class == 69), 2])
m5 <- length(spanish_data[which(spanish_data$class == 68), 2])
m6 <- length(spanish_data[which(spanish_data$class == 20), 2])
m7 <- length(spanish_data[which(spanish_data$class == 47), 2])
m8 <- length(spanish_data[which(spanish_data$class == 23), 2])
m9 <- length(spanish_data[which(spanish_data$class == 4), 2])
m10 <- length(spanish_data[which(spanish_data$class == 39), 2])

total_m <- sum(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10)
total_m

y1 <- mean(spanish_data[which(spanish_data$class == 34), 2])
y2 <- mean(spanish_data[which(spanish_data$class == 65), 2])
y3 <- mean(spanish_data[which(spanish_data$class == 60), 2])
y4 <- mean(spanish_data[which(spanish_data$class == 69), 2])
y5 <- mean(spanish_data[which(spanish_data$class == 68), 2])
y6 <- mean(spanish_data[which(spanish_data$class == 20), 2])
y7 <- mean(spanish_data[which(spanish_data$class == 47), 2])
y8 <- mean(spanish_data[which(spanish_data$class == 23), 2])
y9 <- mean(spanish_data[which(spanish_data$class == 4), 2])
y10 <- mean(spanish_data[which(spanish_data$class == 39), 2])

total_y <- sum(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10)
total_y

t1 <- m1 * y1
t2 <- m2 * y2
t3 <- m3 * y3
t4 <- m4 * y4
t5 <- m5 * y5
t6 <- m6 * y6
t7 <- m7 * y7
t8 <- m8 * y8
t9 <- m9 * y9
t10 <- m10 * y10

total_t <- sum(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
total_t

#
y_bar <- ((total_t) / total_m)
y_bar

m1_2 <- ((m1^2) * ((y1 - y_bar)^2))
m1_2
m2_2 <- ((m2^2) * (y2 - y_bar)^2)
m3_2 <- ((m3^2) * (y3 - y_bar)^2)
m4_2 <- ((m4^2) * (y4 - y_bar)^2)
m5_2 <- ((m5^2) * (y5 - y_bar)^2)
m6_2 <- ((m6^2) * (y6 - y_bar)^2)
m7_2 <- ((m7^2) * (y7 - y_bar)^2)
m8_2 <- ((m8^2) * (y8 - y_bar)^2)
m9_2 <- ((m9^2) * (y9 - y_bar)^2)
m10_2 <- ((m10^2) * (y10 - y_bar)^2)

total_m_2 <- sum(m1_2, m2_2, m3_2, m4_2, m5_2, m6_2, m7_2, m8_2, m9_2, m10_2)
total_m_2

m_bar <- total_m / sample_size

##  standard error
error_2 <- sqrt(((1 - (sample_size) / (population_size)) *
    (1 / ((sample_size) * ((m_bar)^2))) *
    ((total_m_2) / (sample_size - 1))))
error_2

##  confidence interval for part 2b
ci2_1 <- (y_bar - 1.96 * (error_2))
ci2_2 <- (y_bar + 1.96 * (error_2))
ci2 <- c(ci2_1, ci2_2)
ci2

# ==========================================
#               Problem 3
# ==========================================

n <- 6
population_size <- 45

y1 <- c(146, 180, 251, 152, 72, 181, 171, 361, 73, 186)
y2 <- c(99, 101, 52, 121)
y3 <- c(199, 179, 98, 63, 126, 87, 62)
y4 <- c(226, 129, 57, 46, 86, 43, 85, 165)
y5 <- c(12, 23)
y6 <- c(87, 43, 59)

M1 <- length(y1)
M2 <- length(y2)
M3 <- length(y3)
M4 <- length(y4)
M5 <- length(y5)
M6 <- length(y6)
M <- c(M1, M2, M3, M4, M5, M6)


m1 <- 52
m2 <- 19
m3 <- 37
m4 <- 39
m5 <- 8
m6 <- 14
m_total <- sum(m1, m2, m3, m4, m5, m6)

t1 <- sum(y1)
t2 <- sum(y2)
t3 <- sum(y3)
t4 <- sum(y4)
t5 <- sum(y5)
t6 <- sum(y6)
t <- c(t1, t2, t3, t4, t5, t6)

x1_bar <- mean(y1)
x2_bar <- mean(y2)
x3_bar <- mean(y3)
x4_bar <- mean(y4)
x5_bar <- mean(y5)
x6_bar <- mean(y6)

sd1 <- sd(y1)
sd2 <- sd(y2)
sd3 <- sd(y3)
sd4 <- sd(y4)
sd5 <- sd(y5)
sd6 <- sd(y6)

t1_hat <- m1 * x1_bar
t2_hat <- m2 * x2_bar
t3_hat <- m3 * x3_bar
t4_hat <- m4 * x4_bar
t5_hat <- m5 * x5_bar
t6_hat <- m6 * x6_bar
t_total <- sum(t1_hat, t2_hat, t3_hat, t4_hat, t5_hat, t6_hat)

##  question 9 the unbiased estimate for the total number of cases sold
t_hat <- (population_size / n) * (t_total)
t_hat

##  question 11 the average number of cases sold
xbar_hat <- t_total / m_total
xbar_hat

## ratio estimator of the population mean
ybar.r <- sum(t) / sum(M)
ybar.r

##  question 12 standard error y bar 
se.ybar.r <- sqrt((1 - n / population_size)
* sum((t - M * ybar.r)^2)
    / ((n - 1) * sum(M)^2 / n))
se.ybar.r
