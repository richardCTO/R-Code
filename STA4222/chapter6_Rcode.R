
## Number of children for University Professors:
N <- 28
n <- 5

y1 <- c(2,2,4,0,5,0,0,1,1,6)
y2 <- c(1,1,2,0,1,2,0,4,2,2,1,2,1,0,4,3)
y3 <- c(4,5,2,2,0,0,0,2,0,1,1,1,1,1,4,7,3,2,0,0,2)
y4 <- c(3,1,1,0,0,0,2,2,5,4,4,2,1)
y5 <- c(3,1,2,2,1,3,1,1,0,1,2,3,1,2,1,0,5,3,2,1,4,1,2,3,1)

M1 <- length(y1)
M2 <- length(y2)
M3 <- length(y3)
M4 <- length(y4)
M5 <- length(y5)
M <- c(M1,M2,M3,M4,M5)

t1 <- sum(y1)
t2 <- sum(y2)
t3 <- sum(y3)
t4 <- sum(y4)
t5 <- sum(y5)
t <- c(t1,t2,t3,t4,t5)

## ratio estimator of the population mean

ybar.r <- sum(t)/sum(M)
ybar.r

se.ybar.r <- sqrt((1-n/N)*sum((t-M*ybar.r)^2)/((n-1)*sum(M)^2/n))
se.ybar.r

## unbiased estimator of the population total

ybar.unb <- N*mean(t)
ybar.unb

se.ybar.unb <- N*sqrt((1-n/N)*var(t)/n)
se.ybar.unb


## ===============================
## ALF Example

N <- 50
n <- 8
M <- c(17,28,12,25,20,18,15,16)
ages <- list()
ages[[1]] <- c(65,77,69,88,67,70,89)
ages[[2]] <- c(67,90,45,75,43,80,84,67,77,68,75,56)
ages[[3]] <- c(79,49,92,89,50)
ages[[4]] <- c(88,59,78,87,79,95,56,80,89,87)
ages[[5]] <- c(91,70,50,90,90,99,80,79,90)
ages[[6]] <- c(77,82,82,88,74,90,71,45)
ages[[7]] <- c(56,54,83,71,76,75)
ages[[8]] <- c(89,67,69,89,77,60)
m <- sapply(ages,length)
s2 <- sapply(ages,var)


ybar.cls <- vector(length=n)
for(i in 1:n)
{
	ybar.cls[i] <- mean(ages[[i]])
}
that.cls <- ybar.cls*M

ybar.r <- sum(that.cls)/sum(M)
ybar.r

sr2 <- sum((that.cls-M*ybar.r)^2)/(n-1)
var.bcls <- (1-n/N)*sr2/(n*mean(M)^2)
var.wcls <- sum(M^2*(1-m/M)*s2/m)/(n*N*mean(M)^2)
se.ybar.r <- sqrt(var.bcls+var.wcls)
se.ybar.r

 







