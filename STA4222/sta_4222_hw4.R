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

# estimate the total number of students planing a trip

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

# ================= Part B ===================

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
# confidence interval
ci_1 <- (t_hat - 1.96 * (error))
ci_2 <- (t_hat + 1.96 * (error))
ci <- c(ci_1, ci_2)
ci