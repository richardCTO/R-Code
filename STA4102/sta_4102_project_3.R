# ==========================================
#               Problem 
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
ggboxplot(problem1_data,
       x = "GROUP",
       y = "BLOODPRESSURE",
       color = "GROUP",
       palette = c("#00AFBB", "#E7B800"),
       ylab = "BLOOD PRESSURE",
       xlab = "GROUPS"
)

