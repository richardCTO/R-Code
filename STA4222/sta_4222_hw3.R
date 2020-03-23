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

xu <- 10.3
sample_size <- 20
population_size <- 1132
r <- cor(forest_data$Diameter, forest_data$Age)
x <- forest_data$Age
y <- forest_data$Diameter
sx <- sd(forest_data$Diameter)
sy <- sd(forest_data$Age)
ybar <- mean(forest_data$Age)
ybar
xbar <- mean(forest_data$Diameter)
xbar
b1 <- r * sy / sx
b1
b0 <- ybar - b1 * xbar
b0

## 1b ratio estimator
b_hat <- ybar / xbar
b_hat
e <- y - b_hat * x

ybar_r <- b_hat * xu
ybar_r

se_ybar_d2 <- sqrt((1 - sample_size /
    population_size) *
    xu / xbar * (321.93 /
        sample_size))
se_ybar_d2

# 1b error
se_ybar_r <- (xu / mean(x)) * sqrt((1 - sample_size /
    population_size) *
    var(e) / sample_size)

se_ybar_r

# 1c regression estimate population mean
xu <- 10.3
ybar_reg <- b0 + (b1 * xu)
ybar_reg

# 1c standard error of the regression estimate
se_ybar_reg <- sqrt((1 - sample_size / population_size) *
    sy^2 * (1 - r^2) /
    sample_size)
se_ybar_reg

eff_reg <- (se_ybar_r / se_ybar_reg)^2
eff_reg

# plot with regrssion line
abline(lm(Age ~ Diameter, data = forest_data), col = "red")

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
