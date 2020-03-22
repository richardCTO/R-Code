#install.packages("tidyverse")
#install.packages("survey")
library(foreign)
library(survey)
library(tidyverse)

forest_data <- read.csv("STA4222/Data/forest.csv",
    header = TRUE
)
head(forest_data)

ggplot(data = forest_data) + ggtitle("Age vs Diameter") +
    geom_point(mapping = aes(
        x = Diameter,
        y = Age,
    ))

class(forest_data)
head(forest_data)
tail(forest_data)
ncol(forest_data)

# ==========================================
#               Question 1
#===========================================

population_size <- 1132
x_bar <- 10.3
t_x <- x_bar * population_size
x <- forest_data$Diameter
y <- forest_data$Age
x_bar <- mean(x)
x_bar
y_bar <- mean(y)
y_bar
b_hat <-  y_bar / x_bar
b_hat

summary(forest_data[, c("Age", "Diameter")])

mydesign <- svydesign(
    id = ~TreeNo,
    data = forest_data
)
mydesign


svyratio(~Age, ~Diameter, mydesign)
myrations <- svyratio(~Age, ~Diameter, mydesign)
confint(myrations)
cor(forest_data$Age, forest_data$Diameter)

reg_lm <- lm(Diameter ~ Age,
    data = forest_data
)
reg_lm
summary(reg_lm)

# ==========================================
#               Question 2
#===========================================

cherry_data <- read.csv("STA4222/Data/cherry.csv",
    header = TRUE
)
head(cherry_data)

ggplot(data = cherry_data) + ggtitle("Diameter vs. Volume") +
    geom_point(mapping = aes(
        x = diameter,
        y = volume,
    ))
cherry_design <- svydesign(
    id = ~1,
    data = cherry_data
)
cherry_design
cherry_rations <- svyratio(~diameter, ~volume, cherry_design)
confint(cherry_rations)