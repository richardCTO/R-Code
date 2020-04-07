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

# 2a estimate the total number of students planing a trip
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

total <- (m1 + m2 + m3 + m4 + m5 + m6 + m7 + m8 + m9 + m10)
total

n <- 10
# calculate s^2
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
# 2a confidence interval 
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

# standard error
error_2 <- sqrt(((1 - (sample_size) / (population_size)) *
    (1 / ((sample_size) * ((m_bar)^2))) *
    ((total_m_2) / (sample_size - 1))))
error_2

# confidence interval for part 2b
ci2_1 <- (y_bar - 1.96 * (error_2))
ci2_2 <- (y_bar + 1.96 * (error_2))
ci2 <- c(ci2_1, ci2_2)
ci2