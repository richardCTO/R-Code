## Class Survey Example

## read in the data
class.survey <- read.csv("STA4222/Data/ClassSurvey.csv")
## display the first 6 rows of the data
head(class.survey)

## True values of population parameters
## the average course load
ybar.p <- mean(class.survey$Course)
## the total textbooks purchases
t.p <- sum(class.survey$Textbook)
## the proportion of graduation within 6 months
prop.p <- mean(class.survey$Graduation)
N <- length(class.survey$Course)

## draw SRSs
## sample 1 of size 5
set.seed(25630)
sample1 <- sample(class.survey$Index, 5, replace = FALSE)
sample2 <- sample(class.survey$Index, 5, replace = FALSE)
sample3 <- sample(class.survey$Index, 10, replace = FALSE)
n1 <- length(sample1)
n2 <- length(sample2)
n3 <- length(sample3)


## display the selected sample
sample1.dat <- class.survey[class.survey$Index %in% sample1, ]
sample2.dat <- class.survey[class.survey$Index %in% sample2, ]
sample3.dat <- class.survey[class.survey$Index %in% sample3, ]


## ==============================================
## Estimate the population parameters of interst

## Estimate the average course load
sample1.meanest <- mean(sample1.dat$Course)
sample2.meanest <- mean(sample2.dat$Course)
sample3.meanest <- mean(sample3.dat$Course)
sample1.meanest
sample2.meanest
sample3.meanest

## Estimate the variance of the mean estimator
sample1.s2.course <- var(sample1.dat$Course)
sample2.s2.course <- var(sample2.dat$Course)
sample3.s2.course <- var(sample3.dat$Course)
sample1.varest.course <- (1 / n1 - 1 / N) * sample1.s2.course
sample2.varest.course <- (1 / n2 - 1 / N) * sample2.s2.course
sample3.varest.course <- (1 / n3 - 1 / N) * sample3.s2.course
sample1.varest.course
sample2.varest.course
sample3.varest.course


## Estimate the total textbook purchases
sample1.totest <- mean(sample1.dat$Textbook) * N
sample2.totest <- mean(sample2.dat$Textbook) * N
sample3.totest <- mean(sample3.dat$Textbook) * N
sample1.totest
sample2.totest
sample3.totest

## Estimate the variance of the total estimator
sample1.s2.textbook <- var(sample1.dat$Textbook)
sample2.s2.textbook <- var(sample2.dat$Textbook)
sample3.s2.textbook <- var(sample3.dat$Textbook)
sample1.varest.textbook <- (1 / n1 - 1 / N) * sample1.s2.textbook * N^2
sample2.varest.textbook <- (1 / n2 - 1 / N) * sample2.s2.textbook * N^2
sample3.varest.textbook <- (1 / n3 - 1 / N) * sample3.s2.textbook * N^2
sample1.varest.textbook
sample2.varest.textbook
sample3.varest.textbook


## Estimate the proportion of graduation
sample1.propest <- mean(sample1.dat$Graduation)
sample2.propest <- mean(sample2.dat$Graduation)
sample3.propest <- mean(sample3.dat$Graduation)
sample1.propest
sample2.propest
sample3.propest

## Estimate the variance of the proportion estimator
sample1.varest.graduation <- (N - n1) * sample1.propest * (1 - sample1.propest) / ((N - 1) * n1)
sample2.varest.graduation <- (N - n2) * sample2.propest * (1 - sample2.propest) / ((N - 1) * n2)
sample3.varest.graduation <- (N - n3) * sample3.propest * (1 - sample3.propest) / ((N - 1) * n3)
sample1.varest.graduation
sample2.varest.graduation
sample3.varest.graduation

## =========================
## Confidence Intervals

## average course load
round(c(sample1.meanest - qnorm(0.975) * sqrt(sample1.varest.course), sample1.meanest + qnorm(0.975) * sqrt(sample1.varest.course)), 2)
round(c(sample2.meanest - qnorm(0.975) * sqrt(sample2.varest.course), sample2.meanest + qnorm(0.975) * sqrt(sample2.varest.course)), 2)
round(c(sample3.meanest - qnorm(0.975) * sqrt(sample3.varest.course), sample3.meanest + qnorm(0.975) * sqrt(sample3.varest.course)), 2)

## total textbooks
round(c(sample1.totest - qnorm(0.975) * sqrt(sample1.varest.textbook), sample1.totest + qnorm(0.975) * sqrt(sample1.varest.textbook)), 2)
round(c(sample2.totest - qnorm(0.975) * sqrt(sample2.varest.textbook), sample2.totest + qnorm(0.975) * sqrt(sample2.varest.textbook)), 2)
round(c(sample3.totest - qnorm(0.975) * sqrt(sample3.varest.textbook), sample3.totest + qnorm(0.975) * sqrt(sample3.varest.textbook)), 2)

## proportion of graduation
round(c(sample1.propest - qnorm(0.975) * sqrt(sample1.varest.graduation), sample1.propest + qnorm(0.975) * sqrt(sample1.varest.graduation)), 2)
round(c(sample2.propest - qnorm(0.975) * sqrt(sample2.varest.graduation), sample2.propest + qnorm(0.975) * sqrt(sample2.varest.graduation)), 2)
round(c(sample3.propest - qnorm(0.975) * sqrt(sample3.varest.graduation), sample3.propest + qnorm(0.975) * sqrt(sample3.varest.graduation)), 2)


## ======================================
## determin the minimum sample size

## average course load
e1 <- 0.3
S1 <- (6 - 3) / 4
n1.min <- ceiling(1 / ((e1 / (qnorm(0.975) * S1))^2 + 1 / N))
n1.min

## total textbook
e2 <- 10
S2 <- (4 - 0) / 4
n2.min <- ceiling(1 / ((e2 / (qnorm(0.975) * S2 * N))^2 + 1 / N))
n2.min

## proportion of graduation
e3 <- 0.15
p3 <- 0.3
n3.min <- ceiling(N / ((N - 1) * (e3 / qnorm(0.975))^2 / (p3 * (1 - p3)) + 1))
n3.min.cons <- ceiling(N / (4 * (N - 1) * (e3 / qnorm(0.975))^2 + 1))
n3.min
n3.min.cons

