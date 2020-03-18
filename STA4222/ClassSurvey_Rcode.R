## Class Survey Example

## read in the data
class_survey <- read.csv("STA4222/Data/ClassSurvey.csv")
## display the first 6 rows of the data
head(class_survey)

## True values of population parameters
## the average course load
ybar_p <- mean(class_survey$Course)
## the total textbooks purchases
t_p <- sum(class_survey$Textbook)
## the proportion of graduation within 6 months
prop_p <- mean(class_survey$Graduation)
N <- length(class_survey$Course)

## draw SRSs
## sample 1 of size 5
set.seed(25630)
sample1 <- sample(class_survey$Index, 5, replace = FALSE)
sample2 <- sample(class_survey$Index, 5, replace = FALSE)
sample3 <- sample(class_survey$Index, 10, replace = FALSE)
n1 <- length(sample1)
n2 <- length(sample2)
n3 <- length(sample3)



## display the selected sample
sample1_dat <- class_survey[class_survey$Index %in% sample1, ]
sample2_dat <- class_survey[class_survey$Index %in% sample2, ]
sample3_dat <- class_survey[class_survey$Index %in% sample3, ]


## ==============================================
## Estimate the population parameters of interst

## Estimate the average course load
sample1_meanest <- mean(sample1_dat$Course)
sample2_meanest <- mean(sample2_dat$Course)
sample3_meanest <- mean(sample3_dat$Course)
sample1_meanest
sample2_meanest
sample3_meanest

## Estimate the variance of the mean estimator
sample1_s2_course <- var(sample1_dat$Course)
sample2_s2_course <- var(sample2_dat$Course)
sample3_s2_course <- var(sample3_dat$Course)
sample1_varest_course <- (1 / n1 - 1 / N) * sample1_s2_course
sample2_varest_course <- (1 / n2 - 1 / N) * sample2_s2_course
sample3_varest_course <- (1 / n3 - 1 / N) * sample3_s2_course
sample1_varest_course
sample2_varest_course
sample3_varest_course


## Estimate the total textbook purchases
sample2_totest <- mean(sample2_dat$Textbook) * N
sample1_totest <- mean(sample1_dat$Textbook) * N
sample3_totest <- mean(sample3_dat$Textbook) * N
sample2_totest
sample1_totest
sample3_totest

## Estimate the variance of the total estimator
sample1_s2_textbook <- var(sample1_dat$Textbook)
sample2_s2_textbook <- var(sample2_dat$Textbook)
sample3_s2_textbook <- var(sample3_dat$Textbook)
sample1_varest_textbook <- (1 / n1 / N) * sample1_s2_textbook * N ^ 2
sample2_varest_textbook <- (1 / n2 - 1 / N) * sample2_s2_textbook * N ^ 2
sample3_varest_textbook <- (1 / n3 - 1 / N) * sample3_s2_textbook * N ^ 2
sample1_varest_textbook
sample2_varest_textbook
sample3_varest_textbook


## Estimate the proportion of graduation
sample1_propest <- mean(sample1_dat$Graduation)
sample2_propest <- mean(sample2_dat$Graduation)
sample3_propest <- mean(sample3_dat$Graduation)
sample1_propest
sample2_propest
sample3_propest

## Estimate the variance of the proportion estimator
sample1_varest_graduation <- ((N - n1) * sample1_propest *
(1 - sample1_propest) / ((N - 1) * n1))
sample2_varest_graduation <- ((N - n2) * sample2_propest *
(1 - sample2_propest) / ((N - 1) * n2))
sample3_varest_graduation <- ((N - n3) * sample3_propest *
(1 - sample3_propest) /  ((N - 1) * n3))
sample1_varest_graduation
sample2_varest_graduation
sample3_varest_graduation

## =========================
## Confidence Intervals
## average course load
round(c(sample1_meanest - qnorm(0.975) * sqrt(sample1_varest_course),
sample1_meanest + qnorm(0.975) * sqrt(sample1_varest_course)), 2)

round(c(sample2_meanest - qnorm(0.975) * sqrt(sample2_varest_course),
sample2_meanest + qnorm(0.975) * sqrt(sample2_varest_course)), 2)

round(c(sample3_meanest - qnorm(0.975) * sqrt(sample3_varest_course),
sample3_meanest + qnorm(0.975) * sqrt(sample3_varest_course)), 2)

## total textbooks
round(c(sample1_totest - qnorm(0.975) * sqrt(sample1_varest_textbook),
sample1_totest + qnorm(0.975) * sqrt(sample1_varest_textbook)), 2)

round(c(sample2_totest - qnorm(0.975) * sqrt(sample2_varest_textbook),
sample2_totest + qnorm(0.975) * sqrt(sample2_varest_textbook)), 2)

round(c(sample3_totest - qnorm(0.975) * sqrt(sample3_varest_textbook),
sample3_totest + qnorm(0.975) * sqrt(sample3_varest_textbook)), 2)

## proportion of graduation
round(c(sample1_propest - qnorm(0.975) * sqrt(sample1_varest_graduation),
sample1_propest + qnorm(0.975) * sqrt(sample1_varest_graduation)), 2)

round(c(sample2_propest - qnorm(0.975) * sqrt(sample2_varest_graduation),
sample2_propest + qnorm(0.975) * sqrt(sample2_varest_graduation)), 2)

round(c(sample3_propest - qnorm(0.975) * sqrt(sample3_varest_graduation),
sample3_propest + qnorm(0.975) * sqrt(sample3_varest_graduation)), 2)


## ======================================
## determin the minimum sample size
## average course load
e1 <- 0.3
S1 <- (6 - 3) / 4
n1_min <- ceiling(1 / ((e1 / (qnorm(0.975) * S1))^ 2 + 1 / N))
n1_min

## total textbook
e2 <- 10
S2 <- (4 - 0) / 4
n2_min <- ceiling(1 / ((e2 / (qnorm(0.975) * S2 * N))^ 2 + 1 / N))
n2_min

## proportion of graduation
e3 <- 0.15
p3 <- 0.3
n3_min <- ceiling(N / ((N - 1) * (e3 / qnorm(0.975)) ^ 2 / (p3 * (1 - p3)) + 1))
n3_min_cons <- ceiling(N / (4 * (N - 1) * (e3 / qnorm(0.975)) ^ 2 + 1))
n3_min
n3_min_cons