# LDA for OIL data

#Libraries
library(tidyverse)
library(magrittr)
library(MASS)
library(ggord)
library(klaR)
library(psych)
library(heplots)

# Enable the r-universe repo
#options(repos = c(

#fawda123 = 'https://fawda123.r-universe.dev',
#CRAN = 'https://cloud.r-project.org'))

#install.packages('ggord')


# load the data
LDA_data <- read_csv("PCAData.csv")
view(LDA_data)

# Normality Test
df1 <- LDA_data %>% filter(Series == "Pure VCO")
view(df1)
#df1 <- df1 %>% rename(var1 = `3010`)

#shapiro.test(df1$var1)
shapiro.test(df1$`3009`)


# Check for group homogenity (Have to check again)
# Bartlett test
result = bartlett.test(`3002` ~Series , data = LDA_data)
result

# Box M test
box_M <- boxM(df2[, 2:5], df2[, "Series"])
box_M



#LDA
df2 = subset(LDA_data, select = -c(Index, Concentration, Replicate)) 
df2
View(df2) 
lda_results <- lda(Series~., df2)
lda_results

# bi plot 
# ggord(lda_results, df2$Series, ylim = c(-1, 1)) 

# Confusion Matrix 
p1 <- predict(lda_results, df2)$class
View(p1) 
tab1 <- table(Predicted = p1, Actual = df2$Series)
tab1





