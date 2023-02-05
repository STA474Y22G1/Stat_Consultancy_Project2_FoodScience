# Importing data set
pcadata <- read_csv("PCAData.csv")

# data set for box plot
data_boxplot <- pcadata %>% pivot_longer(cols = 5:ncol(pcadata), names_to = "W", 
                                         values_to = "A")

# Box plot
plot <- data_boxplot %>% ggplot(aes(x = A, y = W)) + geom_boxplot() 
plot

# variances are different for each variable. Therefore use correlation matrix for PCA.

# ------------------------------------------------------------------------------

# data set for PCA
pcadata_v2 <- pcadata %>% select(-c(Index:Replicate)) 

pc <- prcomp(pcadata_v2,
             center = TRUE,
             scale. = TRUE)
attributes(pc)

print(pc)



# scree plot
#calculate total variance explained by each principal component
var_explained = pc$sdev^2 / sum(pc$sdev^2)

#create scree plot
qplot(c(1:38), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

# variance explained by each PC
summary(pc)

# Based on scree plot first three PC were selected.
# Based on summary it explains more than 80% variablity.

# extracting PCA scores
Y1 <- pc$x[ , 1]
Y2 <- pc$x[ , 2]
Y3 <- pc$x[ , 3]

# PC data
pcs <- cbind(pcadata[,1:4], Y1, Y2, Y3)

write_csv(pcs, "PC_Scores.csv")














