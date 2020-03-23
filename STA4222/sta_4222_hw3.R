# ==========================================
#               Question 1
#===========================================
# 1a
forest_data <- read.csv("STA4222/Data/forest.csv",
    header = TRUE
)
head(forest_data)

plot(
    forest_data$Diameter, forest_data$Age,
    title(
        main = "Diameter vs Age"
    )
)

population_size <- 1132
x_baru <- 10.3
t_x <- x_baru * population_size
x <- forest_data$Diameter
y <- forest_data$Age
x_bar <- mean(x)
y_bar <- mean(y)
b_hat <-  y_bar / x_bar

#1b
# Ratio estimate of the population mean
ratio_est <- b_hat * x_baru
ratio_est

# 1b error
# calculate residuals
e <- y - b_hat * x
se2 <- sum(e^2) / (n - 1)
se2

## estimate variance of the ratio estimator
se.Bhat <- sqrt(se2 / (n * mean(x)^2))

## calculate 95% CI of the ratio estimator
ci.Bhat <- c(b_hat - qnorm(0.975) * se.Bhat, b_hat + qnorm(0.975) * se.Bhat)
ci.Bhat
# ==========================================
#               Question 2
#===========================================
cherry_data <- read.csv("STA4222/Data/cherry.csv",
    header = TRUE
)
head(cherry_data)


# 2a
plot(
    cherry_data$volume, cherry_data$volume,
    title(
        main = "Diameter vs Volume"
    )
)

# 2b The estimated population total volume
N <- 2967
tx <- 41835

x <- cherry_data$diameter
y <- cherry_data$volume

x_bar <- mean(x)
y_bar <- mean(y)

b_hat <- y_bar / x_bar

ty_r <- b_hat * tx
ty_r

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
#install.packages("Rmisc")
library(Rmisc)
CI(u, ci = .99)

# 3d Estimate the total number of pets
tx <- population_size * .04225
tx
# 3d standard error
