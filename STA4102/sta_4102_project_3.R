# ==========================================
#               Problem 1
# ==========================================
# Loading
library("readxl")
library("readr")   # Fast csv write

my_data <- read_excel("STA4102/Data/Project3.xlsx",
       sheet = "PROBLEM1",
       col_types = c("text", "numeric")
)
head(my_data)

write_csv(my_data, path = "STA4102/Data/Project3.csv")
problem1_data <- read.csv("STA4102/Data/Project3.csv", header = TRUE)
head(problem1_data)

# do the paired t-test
t.test(problem1_data$GROUP == "bp_foreign", problem1_data$GROUP == "bp_us",
       alternative = "two.sided",
       var.equal = FALSE,
       conf.level = 0.95
)

library(dplyr)

group_by(problem1_data, GROUP) %>%
  summarise(
    count = n(),
    mean = mean(BLOODPRESSURE, na.rm = TRUE),
    sd = sd(BLOODPRESSURE, na.rm = TRUE)
  )

# Plot weight by group and color by group
library("ggpubr")
ggboxplot(
       problem1_data,
       x = "GROUP",
       y = "BLOODPRESSURE",
       color = "GROUP",
       palette = c("#00AFBB", "#E7B800"),
       ylab = "BLOOD PRESSURE",
       xlab = "GROUPS"
)

# ==========================================
#               Problem 2
# ==========================================

# Loading
library("readxl")
library("readr")   # Fast csv write

my_data2 <- read_excel("STA4102/Data/Project3.xlsx",
       sheet = "PROBLEM2",
       col_types = c("text", "numeric")
)
head(my_data2)

write_csv(my_data2, path = "STA4102/Data/Project3_Problem2.csv")
problem2_data <- read.csv("STA4102/Data/Project3_Problem2.csv", header = TRUE)
head(problem2_data)

# Compute the analysis of variance
res_aov <- aov(DAYS_TO_DEATH ~ TREATMENT, data = problem2_data)
# Summary of the analysis
summary(res_aov)

# ==========================================
#               Problem 3
# ==========================================

# Loading
library("readxl")
library("readr")   # Fast csv write

my_data3 <- read_excel("STA4102/Data/Project3.xlsx",
       sheet = "PROBLEM3",
       col_types = c("numeric", "numeric")
)
head(my_data3)

write_csv(my_data3, path = "STA4102/Data/Project3_Problem3.csv")
problem3_data <- read.csv("STA4102/Data/Project3_Problem3.csv", header = TRUE)
head(problem3_data)

# build linear regression model on full data
l_reg <- lm(calories ~ systolic_bp, data = problem3_data)
print(l_reg)

summary(l_reg)

# scatterplot
scatter.smooth(
       x = problem3_data$calories,
       y = problem3_data$systolic_bp,
       main = "Calories ~ Systolic BP"
)

# ==========================================
#               Problem 4
# ==========================================

# Loading
library("readxl")
library("readr") # Fast csv write

# read excel file
my_data4 <- read_excel("STA4102/Data/Project3.xlsx",
       sheet = "PROBLEM4",
       col_types = c("numeric", "numeric")
)
# print first 6 rows of data set to confirm corect output
head(my_data4)

# convert excel file into csv file for easier data manipulation
write_csv(my_data4, path = "STA4102/Data/Project3_Problem4.csv")
problem4_data <- read.csv("STA4102/Data/Project3_Problem4.csv", header = TRUE)

# print first 6 rows of new csv file
head(problem4_data)