# PLS Regression
library(tidyverse)
library(pls)

# Importing dataset
PLS_data <- read_csv("PCAData.csv")

# Coverting concentration to numeric vector
PLS_data <- PLS_data %>% separate(Concentration, into = c("Concentration", "percent"))
PLS_data$Concentration <- as.numeric(PLS_data$Concentration)/100

PLS_data <- PLS_data %>% filter(Series == "Adulterated") %>% select(c(Concentration, "3010":"1105"))

# Split the column names in X and Y
X_colnames <- colnames(PLS_data)[2:39]
Y_colnames <- colnames(PLS_data)[1]

# Split train data into matrices
X_train_matrix <- as.matrix(PLS_data[X_colnames])
Y_train_matrix <- as.matrix(PLS_data[Y_colnames])

pls <- plsr(Y_train_matrix ~ X_train_matrix, scale=TRUE, validation="CV")

summary(pls)


# Create a plot to define the number of components
plot(RMSEP(pls))

# # Plot to see how the regression coefficients are different based on the number
# # of components retained
# plot(pls, plottype = "coef", ncomp = c(1:7), legendpos = "bottomleft")

#visualize cross-validation plots
validationplot(pls)
validationplot(pls, val.type="MSEP")
validationplot(pls, val.type="R2")

# prediction
pcr_pred <- predict(pls, PLS_data[,2:39], ncomp=8)

#calculate RMSE
sqrt(mean((pcr_pred - PLS_data$Concentration)^2))


palm_data <- read_csv("Tidy Data.csv")
view(palm_data)

palm_data <- palm_data %>% filter(Series == "Pure Palm Oil")

























