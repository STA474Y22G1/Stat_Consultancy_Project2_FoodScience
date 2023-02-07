# checking outliers of pcs

pc_data <- read_csv("PC_Scores.csv")

View(pc_data)

data_pca_outlier_checking <- pc_data %>% pivot_longer(cols = 5:7, names_to = "PC", 
                                                      values_to = "Value")

plot1 <- data_pca_outlier_checking %>% ggplot(aes(x = PC, y = Value)) + geom_boxplot() 
plot1

ggplotly(plot1)

mahalanobis_data <- pc_data %>% select(Y1, Y2, Y3)

mahalanobis_data$mahalanobis <- mahalanobis(mahalanobis_data, 
                                            colMeans(mahalanobis_data), 
                                            cov(mahalanobis_data))

mahalanobis_data$pvalue <- pchisq(mahalanobis_data$mahalanobis, df=2, lower.tail=FALSE)

mahalanobis_data <- cbind(pc_data[,1:4], mahalanobis_data)

View(mahalanobis_data)

# write_csv(mahalanobis_data, "mahalanobis_data.csv")

# removing outliers

outlier_removed_pc <- mahalanobis_data %>% filter(pvalue > 0.001)
View(outlier_removed_pc)

write_csv(outlier_removed_pc, "outlier_removed_pc.csv")

