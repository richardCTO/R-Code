# ==========================================
#               Question 3
#===========================================
population_size <- 480000
sample_size <- 940

num_pets <- c(0, 1, 2, 3, 4, 5, 8, 10)
num_respondents  <- c(550, 320, 45, 15, 6, 2, 1, 1)
n1 <- sum(num_respondents)

# 3a estimate proportion
sample_size <- 940
x_bar <- (320 + 45 + 15 + 6 + 2 + 1 + 1)
y <- sample_size

p_hat <- x_bar / y
p_hat

# 3a standard error
error <- sqrt((p_hat * (1 - p_hat)) / n1)
error # standard error

# 3b estimate domain mean
u <- c(
    rep(0, 550), rep(1, 320),
    rep(2, 45), rep(3, 15), rep(4, 6),
    rep(5, 2), rep(8, 1), rep(10, 1)
)
ybar_domain <- mean(u) / (n1 / sample_size)
ybar_domain

# 3b standard error
standard_error <- function(x) sd(x) / sqrt(length(x))
standard_error(u)

# 3c calculate 99% CI of the ratio estimator
install.packages("Rmisc")
library(Rmisc)
CI(u, ci = .99)

# 3d Estimate the total number of pets
tx <- population_size * .04225
tx
# 3d standard error