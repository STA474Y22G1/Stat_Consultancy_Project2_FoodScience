# LDA for OIL data

#Libraries
library(tidyverse)
library(MASS)

# load the data
LDA_data <- read_csv("PCAData.csv")
view(LDA_data)

# Normality Test
df1 <- LDA_data %>% filter(Series == "Pure VCO")
view(df1)
#df1 <- df1 %>% rename(var1 = `3010`)

#shapiro.test(df1$var1)
shapiro.test(df1$`3009`)



#check for group homogenity - Bartlett test
result = bartlett.test(`3002` ~Series , 
                       data = LDA_data)
result



#LDA
df2 <- LDA_data%>% select(-c("Index",  "Concentration", "Replicate"))
view(df2)
lda_results <- lda(Series~., df2)
lda_results


