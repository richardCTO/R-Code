cherry_data <- read.csv("STA4222/Data/cherry.csv",
    header = TRUE
)
head(cherry_data)

total_size_n <- 2967
sample_size_n <- 31
t_sub_x <- 41835
x_sub_u <- t_sub_x / total_size_n

y <- mean(cherry_data$volume)
x <- mean(cherry_data$diameter)
b_hat <- x / y
b_hat

error <- y - b_hat * x
standard_error_2 <- sum(error^2) / (total_size_n - 1)

## estimate variance of the ratio estimator
se_b_hat <- sqrt(standard_error_2 / (total_size_n * mean(x)^2))
se_b_hat
## calculate 95% CI of the ratio estimator
ci_b_hat <- c(
    b_hat - qnorm(0.975) * se_b_hat,
    b_hat + qnorm(0.975) * se_b_hat
)
ci_b_hat