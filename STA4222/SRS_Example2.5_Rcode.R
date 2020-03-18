
# Lohr (2010) page 34 Example 2.5

# read in the sampling frame of the population
frame <- read.csv("/home/richard/Documents/R Code/STA4222/Data/agsrs.csv", header = T)
head(frame)
N <- dim(frame)[1]
hist(frame$acres92, col = "gray", xlab = "Acres Devoted to Farms", main = "Histogram of the Total Acreage Devoted to Farms")

# draw a simple random sample of size n=300
set.seed(863849)
n <- 300
srssam <- frame[sample(1:N, n, replace = FALSE),]
head(srssam)

# estimate population total for acres92

# sample mean
ybar <- mean(srssam[, "acres92"])
ybar

# Use sampling weights
srssam$sampwts <- N / n
ybar2 <- sum(srssam$acres92 * srssam$sampwts) / N
ybar2

# est. population total
that <- N * ybar
that

# Use sampling weights
that2 <- sum(srssam$acres92 * srssam$sampwts)
that2

# sample SD
s <- sd(srssam[, "acres92"])

# SE of est. population mean
se_ybar <- sqrt(1 / n - 1 / N) * s
se_ybar

# SE of est. population total
se_that <- N * se_ybar
se_that

# est. CV for est. populaiton mean
cvhat_ybar <- se_ybar / ybar
cvhat_ybar

# est. CV for est. populaiton total
cvhat_that <- se_that / that
cvhat_that


## =======================
# To estimate the proportion of farms smaller than 200K acres
# define binary variable
srssam$lt200k <- as.numeric(srssam$acres92 < 200000)
head(srssam)
# est. of proportion
phat <- mean(srssam[, "lt200k"])
phat

# Use sampling weights
phat2 <- sum(srssam$lt200k * srssam$sampwts) / N
phat2

# SE of est. proportion
se_phat <- sqrt((phat * (1 - phat) / n) * (N - n) / (N - 1))
se_phat

# est. CV for est. proportion
cvhat_phat <- se_phat / phat
cvhat_phat
