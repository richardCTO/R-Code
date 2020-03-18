library(boot)
source("c:/courses/st446/rcode/confintt.r")
indata <- read.table("c://courses/st446/Rcode/agsrs.txt", header = T)

y <- indata$ACRES92
x <- indata$ACRES87
ratio <- cbind(x, y)
ratio <- data.frame(ratio)

N <- 3068 # population size
Brep <- 20000
tx <- 964470625 # X population total if known
mux <- tx / N # X population mean if known

# Bootstrap the sample ratio
sampratio <- function(ratio, i) mean(y[i] / mean(x[i]))
bootratio <- boot(data = ratio, statistic = sampratio, R = Brep)
bootratio
boot.ci(bootratio, conf = .95, type = c("norm", "perc"))
par(mfrow = c(2, 1))
hist(bootratio$t, main = "Bootstrap Sample Ratios")
plot(ecdf(bootratio$t), main = "Empirical CDF of Bootstrap Ratios")

# Bootstrap the estimates of the y population mean (tx known)
sampmean <- function(ratio, i) mux * mean(y[i]) / mean(x[i])
bootmean <- boot(data = ratio, statistic = sampmean, R = Brep)
bootmean
boot.ci(bootmean, conf = .95, type = c("norm", "perc"))
par(mfrow = c(2, 1))
hist(bootmean$t, main = "Bootstrap y Population Mean Estimates")
plot(ecdf(bootmean$t), main = "Empirical CDF of Bootstrap Mean Estimates")

# Bootstrap the estimates of the y population total (tx known)
samptotal <- function(ratio, i) tx * mean(y[i]) / mean(x[i])
boottotal <- boot(data = ratio, statistic = samptotal, R = Brep)
boottotal
boot.ci(boottotal, conf = .95, type = c("norm", "perc"))
par(mfrow = c(2, 1))
hist(boottotal$t, main = "Bootstrap y Population Total Estimates")
plot(ecdf(boottotal$t), main = "Empirical CDF of Bootstrap Total Estimates")