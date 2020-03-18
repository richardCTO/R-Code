## ===========================
## Agriculture Survey Example
## ===========================

pop.frame <- read.csv("",
    header = TRUE
)
N <- dim(pop.frame)[1]
tx <- 964470625
xu <- tx / N

srs.sam <- read.csv("",
    header = TRUE
)
n <- dim(srs.sam)[1]
plot(srs.sam$acres87, srs.sam$acres92,
    xlab = "Millions of Acres Devoted to Farms (1987)",
    ylab = "Millions of Acres Devoted to Farms (1992)"
)
cor(srs.sam$acres92, srs.sam$acres87)

## estimate the ratio
Bhat <- mean(srs.sam$acres92) / mean(srs.sam$acres87)
Bhat
## calculate the residuals
e <- srs.sam$acres92 - Bhat * srs.sam$acres87

## estimate variance of the ratio estimator
Vhat.Bhat <- (1 - n / N) * var(e) / (n * mean(srs.sam$acres87)^2)
SE.Bhat <- sqrt(Vhat.Bhat)
SE.Bhat

## estimate the population mean acreage
yhat.r <- Bhat * xu
yhat.r

## estimate variance of the estimated population mean
SE.yhat.r <- SE.Bhat * xu
SE.yhat.r

## estimate the population total acreage
that.r <- Bhat * tx
that.r

## estimate variance of the estimated population mean
SE.that.r <- SE.Bhat * tx
SE.that.r

## using sampling weigts & calibration factor
srs.sam$ws <- N / n
txhat <- N * mean(srs.sam$acres87)
srs.sam$gs <- tx / txhat

# compare to calibrated population total
sum(srs.sam$ws * srs.sam$gs * srs.sam$acres87)
tx

# compare to calibrated population size
sum(srs.sam$ws * srs.sam$gs)
N

## estimate the population total
that.r2 <- sum(srs.sam$ws * srs.sam$gs * srs.sam$acres92)
that.r2

## estimate variance 


## ==========================
## Tree Seedlings Example
## ==========================

y <- c(0, 0, 1, 2, 10, 15, 3, 2, 1, 27)
x <- c(1, 0, 8, 2, 76, 60, 25, 2, 1, 31)
n <- length(y)

## estimate the ratio
Bhat <- mean(y) / mean(x)

## calculate residuals
e <- y - Bhat * x
se2 <- sum(e^2) / (n - 1)

## estimate variance of the ratio estimator
se.Bhat <- sqrt(se2 / (n * mean(x)^2))

## calculate 95% CI of the ratio estimator
ci.Bhat <- c(Bhat - qnorm(0.975) * se.Bhat, Bhat + qnorm(0.975) * se.Bhat)



## ======================
# Boat Owner Example
## ======================
n <- 1500
N <- 400000
num.child <- c(0, 1, 2, 3, 4, 5, 6, 8)
num.owners <- c(76, 139, 166, 63, 19, 5, 3, 1)
n1 <- sum(num.owners)

# estimate domain mean
# Approach 1
u <- c(
    rep(0, 1104), rep(1, 139),
    rep(2, 166), rep(3, 63), rep(4, 19),
    rep(5, 5), rep(6, 3), rep(8, 1)
)
ybar.d1 <- mean(u) / (n1 / n)
ybar.d1
# Approach 2
ybar.d2 <- sum(num.child * num.owners) / n1
ybar.d2
res <- (num.child - ybar.d2)^2
se2 <- sum(res * num.owners) / (n - 1)
se.ybar.d2 <- sqrt((1 - n / N) * se2 / (n * (n1 / n)^2))
se.ybar.d2

# estimate domain total
u <- c(
    rep(0, 1104),rep(1, 139), 
    rep(2, 166),rep(3, 63), 
    rep(4, 19),rep(5, 5), 
    rep(6, 3),rep(8, 1)
)
that.d <- N * mean(u)
that.d
se.that.d <- N * sqrt((1 - n / N) * var(u) / n)
se.that.d

## ======================
## Dead Trees Example
## ======================

trees <- read.csv("", header = T)
plot(trees$photo, trees$field,
    xlab = "Phote Count of Dead Trees",
    ylab = "Field Count of Dead Trees",
    pch = 16,
    cex = 1.2
)

n <- dim(trees)[1]
N <- 100
r <- cor(trees$photo, trees$field)
sx <- sd(trees$photo)
sy <- sd(trees$field)
ybar <- mean(trees$field)
xbar <- mean(trees$photo)
b1 <- r * sy / sx
b0 <- ybar - b1 * xbar

fit1 <- lm(field ~ photo, data = trees)
summary(fit1)
anova(fit1)
plot(trees$photo, trees$field,
    xlab = "Phote Count of Dead Trees",
    ylab = "Field Count of Dead Trees",
    pch = 16,
    cex = 1.2
)
abline(fit1, lwd = 2, lty = 2, col = "red")

xu <- 11.3
ybar.reg <- b0 + b1 * xu
ybar.reg
se.ybar.reg <- sqrt((1 - n / N) * sy^2 * (1 - r^2) / n)
se.ybar.reg
se.ybar.reg2 <- sqrt((1 - n / N) * deviance(fit1) / (n * (n - 1)))
se.ybar.reg2

## ratio estimator
bhat <- ybar / xbar
bhat

e <- trees$field - bhat * trees$photo
ybar.r <- bhat * xu
ybar.r

se.ybar.r <- (xu / mean(trees$photo)) * sqrt((1 - n / N) * var(e) / n)
se.ybar.r

eff.reg <- (se.ybar.r / se.ybar.reg)^2
eff.reg