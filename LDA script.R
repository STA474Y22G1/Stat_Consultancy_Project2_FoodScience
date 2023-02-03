# LDA for OIL data

#Libraries
library(tidyverse)

# load the data
LDA_data <- read_csv("PCAData.csv")


# Normality Test
df1 <- LDA_data %>% filter(Series == "Pure VCO")
#df1 <- df1 %>% rename(var1 = `3010`)

shapiro.test(df1$var1)
shapiro.test(df1$`3009`)

# var-cov matrix
df2 <- df1 %>% select(-c("Index", "Series", "Concentration", "Replicate"))
