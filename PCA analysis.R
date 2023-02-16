# Importing data set
pcadata <- read_csv("PCAData.csv")

# data set for box plot
data_boxplot <- pcadata %>% pivot_longer(cols = 5:ncol(pcadata), names_to = "W", 
                                         values_to = "A")

# Box plot
plot <- data_boxplot %>% ggplot(aes(x = A, y = W)) + geom_boxplot() +
  xlab("Absorption") + ylab("Wave Number (cm-1)")
plot

# variances are different for each variable. Therefore use correlation matrix for PCA.

# ------------------------------------------------------------------------------

# data set for PCA
pcadata_v2 <- pcadata %>% subset(select = -c(Index, Series, Concentration, Replicate))

pc <- prcomp(pcadata_v2,
             center = TRUE,
             scale. = TRUE)
attributes(pc)

print(pc) # PC Coefficents



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
pcs <- cbind(pcadata[,1:4], Y1, Y2, Y3) %>% as.data.frame()


mahalanobis_data <- pcs %>% subset(select = c(Y1, Y2, Y3))

mahalanobis_data$mahalanobis <- mahalanobis(mahalanobis_data, 
                                            colMeans(mahalanobis_data), 
                                            cov(mahalanobis_data))

mahalanobis_data$pvalue <- pchisq(mahalanobis_data$mahalanobis, df=2, lower.tail=FALSE)

mahalanobis_data <- cbind(pcs[,1:4], mahalanobis_data)

# removing outliers

outlier_removed_pc <- mahalanobis_data %>% filter(pvalue > 0.001)
View(outlier_removed_pc)

write_csv(outlier_removed_pc, "outlier_removed_pc.csv")


test_pcs <- predict(pc, newdata = pcadata_v2)
test_pcs3 <- data.frame(Y1 = test_pcs[,1],
                        Y2 = test_pcs[,2],
                        Y3 = test_pcs[,3])
test_pcs3 <- as.data.frame(cbind(pcadata$Series, test_pcs3))

# Confusion Matrix 
pred <- predict(qda_results, test_pcs3)$class



# Biplot
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
g <- ggbiplot(pc, obs.scale = 1, var.scale = 1, 
              groups = pcadata$Series, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

g2 <- ggbiplot(pc, choices = 2:3, obs.scale = 1, var.scale = 1, 
              groups = pcadata$Series, ellipse = TRUE, 
              circle = TRUE)
g2 <- g2 + scale_color_discrete(name = '')
g2 <- g2 + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)








