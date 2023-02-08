# Importing dataset
PLS_data <- read_csv("PCAData.csv")

PLS_data <- PLS_data %>% pivot_longer(5:42, names_to = "Wave.No", values_to = "Absorbtion")

# Coverting concentration to numeric vector
PLS_data <- PLS_data %>% separate(Concentration, into = c("Concentration", "percent"))
PLS_data$Concentration <- as.numeric(PLS_data$Concentration)

scatter.smooth(PLS_data$Concentration, PLS_data$Absorbtion, main='Concentration vs. Absorbtion')

#fit simple linear regression model
model <- lm(Absorbtion~Concentration, data = PLS_data)

#view model summary
summary(model)

#define residuals
res <- resid(model)

#produce residual vs. fitted plot
plot(fitted(model), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res)

#==============================================================================

#fit simple linear regression model
model1 <- lm(Concentration~Absorbtion, data = PLS_data)

#view model summary
summary(model1)

#define residuals
res <- resid(model1)

#produce residual vs. fitted plot
plot(fitted(model1), res)

#add a horizontal line at 0 
abline(0,0)

#create Q-Q plot for residuals
qqnorm(res)

#add a straight diagonal line to the plot
qqline(res)

shapiro.test(res)
