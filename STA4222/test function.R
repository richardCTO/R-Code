
library("scatterplot3d") # load

data(iris)
head(iris)

# Basic 3d graphics
scatterplot3d(iris[, 1:3])

shiny::runExample("01_hello")