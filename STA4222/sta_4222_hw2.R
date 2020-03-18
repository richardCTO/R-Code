################## Question 1 ##################
# read in the dataset
# i also added header names for the dataset
baseball_data <- read.csv("STA4222/Data/baseball.csv",
    header = TRUE,
    stringsAsFactors = FALSE
)
##### Part a #####
#print the first 6 rows of the dataset
head(baseball_data)
# set seed
set.seed(12345678)
# systematic sample of 150 out of 797, done so by
# choosing every 5th row
baseball_sys_150 <- baseball_data[seq(
    from = 1,
    to = nrow(baseball_data),
   length.out = 150
), ]
# print the first 6 rows of the new systematic sample
head(baseball_sys_150)
baseball_sys_150
# print the last 6 rows of the new systematic sample
tail(baseball_sys_150)
# print the dimension of the new systematic sample
dim(baseball_sys_150)

##### Part b #####
# mean of all baseball salaries
mean(baseball_data$salary)
# mean and confidence interval for salary from SYS of 150
baseball_sys_mean_ci <- baseball_sys_150$salary
t.test(baseball_sys_mean_ci)

##### Part c #####
# convert data into numeric values
baseball_numeric <- as.numeric(as.factor(baseball_sys_150$position))
# assign the numeric values of positions in new column
baseball_sys_150$position_numeric <- baseball_numeric
# put only the pitchers into a new column
baseball_sys_150$pitchers <- baseball_sys_150$position_numeric == 7
# print first 6 rows to make sure everything is correct
head(baseball_sys_150)
# print all rows to make sure everything is working
baseball_sys_150
# est. of proportion of pitchers
p_hat <- mean(as.numeric(baseball_sys_150[, "pitchers"]))
# print est. of proportion for pitchers
p_hat