# PLS Regression
library(tidyverse)
library(pls)

# Importing dataset
PLS_data <- read_csv("PCAData.csv")

# Coverting concentration to numeric vector
PLS_data <- PLS_data %>% separate(Concentration, into = c("Concentration", "percent"))
PLS_data$Concentration <- as.numeric(PLS_data$Concentration)
PLS_data <- PLS_data %>% select(c(Concentration, "3010":"1105"))

# Split the column names in X and Y
X_colnames <- colnames(PLS_data)[2:39]
Y_colnames <- colnames(PLS_data)[1]

# Split train data into matrices
X_train_matrix <- as.matrix(PLS_data[X_colnames])
Y_train_matrix <- as.matrix(PLS_data[Y_colnames])

pls <- plsr(Y_train_matrix ~ X_train_matrix, ncomp = 38, scale = TRUE)

# Create a plot to define the number of components
plot(RMSEP(pls))

# Plot to see how the regression coefficients are different based on the number
# of components retained
plot(pls, plottype = "coef", ncomp = c(1:3), legendpos = "bottomleft")
