# LDA for OIL data

#Libraries
library(tidyverse)
library(magrittr)
library(MASS)
#library(ggord)
library(klaR)
library(psych)
library(heplots)
library(car)

# Enable the r-universe repo
# options(repos = c(
# 
# fawda123 = 'https://fawda123.r-universe.dev',
# CRAN = 'https://cloud.r-project.org'))
# 
# install.packages('ggord')


# load the data
#LDA_data <- read_csv("outlier_removed_pc.csv")
LDA_data <- read_csv("PC_Scores.csv")
view(LDA_data)


# Normality Test
# Group into Pure VCO and Adulterated
Pure_VCO <- LDA_data %>% filter(Series == "Pure VCO")
Adulterated <- LDA_data %>% filter(Series == "Adulterated")


# Apply shapiro test to check the Normality
shapiro.test(Pure_VCO$Y1)
shapiro.test(Pure_VCO$Y2)
shapiro.test(Pure_VCO$Y3)

shapiro.test(Adulterated$Y1)
shapiro.test(Adulterated$Y2)
shapiro.test(Adulterated$Y3)


# Checking the Assumption of Equal Covariance 
# Box M test
# boxm <- heplots::boxM(LDA_data[, c(5:7)], LDA_data$Series)
# boxm

# Levene’s test
levene_data <- rbind(Pure_VCO, Adulterated)
levene_result_Y1 = leveneTest(Y1 ~ Series, levene_data)
levene_result_Y2 = leveneTest(Y2 ~ Series, levene_data)
levene_result_Y3 = leveneTest(Y3 ~ Series, levene_data)

# print the result
print(result)



#LDA
LDA_test_data = subset(LDA_data, select = -c(Index, Concentration, Replicate)) 
 
lda_results <- lda(Series~., LDA_test_data)
lda_results

# bi plot 
# ggord(lda_results, df2$Series, ylim = c(-1, 1)) 

# Confusion Matrix 
pred <- predict(lda_results, LDA_test_data)$class
 
confusion_matrix <- table(Predicted = pred, Actual = LDA_test_data$Series)
confusion_matrix


###################################################################################

# QDA
# Didn't use training set and testing set

QDA_test_data = subset(LDA_data, select = -c(Index, Concentration, Replicate)) 

qda_results <- qda(Series~., QDA_test_data)
qda_results

# bi plot 
# ggord(lda_results, df2$Series, ylim = c(-1, 1)) 

# Confusion Matrix 
pred <- predict(qda_results, QDA_test_data)$class

confusion_matrix <- table(Predicted = pred, Actual = QDA_test_data$Series)
confusion_matrix



