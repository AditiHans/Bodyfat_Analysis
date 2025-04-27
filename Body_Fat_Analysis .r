# =============================
# Body Fat Analysis - Final R Script
# =============================

#1. Load Required Libraries
library(ggplot2)
library(corrplot)
library(gridExtra)
library(grid)

#2. Load & Clean Data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))# Choosing directory
getwd()
bodyfat_data <- read.csv("./bodyfat.csv")#opening file

# Remove missing values
bodyfat_data <- na.omit(bodyfat_data)

#<-------cleaning end----------->

#3. Summary Statistic

# Compute Summary Statistics
min_vals <- sapply(bodyfat_data[sapply(bodyfat_data, is.numeric)], min, na.rm = TRUE)
q1_vals <- sapply(bodyfat_data[sapply(bodyfat_data, is.numeric)], quantile, probs = 0.25, na.rm = TRUE)
median_vals <- sapply(bodyfat_data[sapply(bodyfat_data, is.numeric)], median, na.rm = TRUE)
mean_vals <- sapply(bodyfat_data[sapply(bodyfat_data, is.numeric)], mean, na.rm = TRUE)
q3_vals <- sapply(bodyfat_data[sapply(bodyfat_data, is.numeric)], quantile, probs = 0.75, na.rm = TRUE)
max_vals <- sapply(bodyfat_data[sapply(bodyfat_data, is.numeric)], max, na.rm = TRUE)
sd_vals <- sapply(bodyfat_data[sapply(bodyfat_data, is.numeric)], sd, na.rm = TRUE)

# Manually calculate kurtosis for each numeric column
kurtosis_vals <- sapply(bodyfat_data[sapply(bodyfat_data, is.numeric)], function(x) {
  n <- length(x)
  mean_x <- mean(x, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  fourth_moment <- mean((x - mean_x)^4, na.rm = TRUE)
  kurtosis <- (fourth_moment / (sd_x^4)) - 3
  return(kurtosis)
})

# Combine results into a data frame
summary_stats <- data.frame(
  Variable = names(bodyfat_data[sapply(bodyfat_data, is.numeric)]),
  Min = min_vals,
  Q1 = q1_vals,
  Median = median_vals,
  Mean = mean_vals,
  Q3 = q3_vals,
  Max = max_vals,
  SD = sd_vals,
  Kurtosis = kurtosis_vals
)

# Saving as PNG
png("Summary_Statistics.png", width = 1000, height = 600)
grid.table(summary_stats)
dev.off()

#<---------summary stats end---------->

#4. Compute Correlation Matrix & Heatmap

# Computing Correlation Matrix
cor_matrix <- cor(bodyfat_data[, sapply(bodyfat_data, is.numeric)], use = "complete.obs")

# Saving Heatmap as PNG
png("Correlation_Heatmap.png", width = 1000, height = 800)  

# Generating Optimized Heatmap
corrplot(cor_matrix, 
         method = "color",   
         type = "full",       
         col = colorRampPalette(c('#ff7f50', '#ff6b6b', '#ff4757', '#ff6b81', '#ffa07a', '#ff8c00', '#ff6347'))(200), # Custom color palette
         tl.col = "black",    
         tl.srt = 45,         
         tl.cex = 1.5,        
         number.cex = 1.5,    
         addCoef.col = "black", 
         mar = c(0, 0, 0, 0), 
         diag = TRUE,
         cl.cex = 1)

dev.off()
#<-----correlation analysis end-------->

#5. highest correlation values & comparing highly correlated values using scatterplot

# Finding correlations greater than 0.7 or less than -0.7
high_corr_pairs <- which(abs(cor_matrix) > 0.7 & cor_matrix != 1, arr.ind = TRUE)
print(high_corr_pairs)

# Select Top 4 Most Correlated Pairs for Plotting
selected_pairs <- list(
  c("Abdomen", "BodyFat"),
  c("Weight", "Hip"),
  c("Thigh", "Hip"),
  c("Chest", "Neck")  
)

# Create Scatter Plots for Selected Correlated Pairs
plots <- list()

for (pair in selected_pairs) {
  p <- ggplot(bodyfat_data, aes_string(x = pair[1], y = pair[2])) +
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red") +
    labs(title = paste("Scatter Plot:", pair[1], "vs", pair[2]), 
         x = pair[1], 
         y = pair[2]) +
    theme_minimal()
  
  plots <- append(plots, list(p))
}

# Saving Multi-Panel Scatter Plot Figure
png("Multi_Scatter_Plots.png", width = 1500, height = 1000)
grid.arrange(grobs = plots, ncol = 2)  
dev.off()

#<---------correlated coefficient analysis end----------> 

#6.Variables influence on body fat using Linear Regression
simple_model <- lm(BodyFat ~ Abdomen, data = bodyfat_data)
multi_model <- lm(BodyFat ~ Abdomen + Neck + Wrist, data = bodyfat_data)
summary(simple_model)
summary(multi_model)

#<-----Regression Analysis end---------->

#7. Hypothesis Testing (T-tests & Wilcoxon)
# Computing median Body Fat
median_fat <- median(bodyfat_data$BodyFat, na.rm = TRUE)

# Creating categorical variable: High vs Low Body Fat
bodyfat_data$Bodyfat_Split <- ifelse(bodyfat_data$BodyFat > median_fat, "High", "Low")

# Converting to factor
bodyfat_data$Bodyfat_Split <- as.factor(bodyfat_data$Bodyfat_Split)

# Checking distribution of groups
table(bodyfat_data$Bodyfat_Split)

# T-test for Abdomen
t.test(Abdomen ~ Bodyfat_Split, data = bodyfat_data)

# T-test for Neck
t.test(Neck ~ Bodyfat_Split, data = bodyfat_data)

# T-test for Wrist
t.test(Wrist ~ Bodyfat_Split, data = bodyfat_data)

# Wilcoxon test for Abdomen
wilcox.test(Abdomen ~ Bodyfat_Split, data = bodyfat_data)

# Wilcoxon test for Neck
wilcox.test(Neck ~ Bodyfat_Split, data = bodyfat_data)

# Wilcoxon test for Wrist
wilcox.test(Wrist ~ Bodyfat_Split, data = bodyfat_data)

# Saving Boxplot for Abdomen by Body Fat Group
png("Boxplot_Abdomen_vs_BodyFat.png", width = 1000, height = 800)
ggplot(bodyfat_data, aes(x = Bodyfat_Split, y = Abdomen, fill = Bodyfat_Split)) +
  geom_boxplot() +
  labs(title = "Abdomen Circumference by Body Fat Category", x = "Body Fat Category", y = "Abdomen (cm)")
dev.off()

# Saving Boxplot for Neck by Body Fat Group
png("Boxplot_Neck_vs_BodyFat.png", width = 1000, height = 800)
ggplot(bodyfat_data, aes(x = Bodyfat_Split, y = Neck, fill = Bodyfat_Split)) +
  geom_boxplot() +
  labs(title = "Neck Circumference by Body Fat Category", x = "Body Fat Category", y = "Neck (cm)")
dev.off()

# Saving Boxplot for Wrist by Body Fat Group
png("Boxplot_Wrist_vs_BodyFat.png", width = 1000, height = 800)
ggplot(bodyfat_data, aes(x = Bodyfat_Split, y = Wrist, fill = Bodyfat_Split)) +
  geom_boxplot() +
  labs(title = "Wrist Size by Body Fat Category", x = "Body Fat Category", y = "Wrist (cm)")
dev.off()
#<------------Hypothesis Testing end------------->

#8. ANOVA & Tukey Test for Age Groups

age_breaks <- quantile(bodyfat_data$Age, probs = c(0, 0.25, 0.50, 0.75, 1), na.rm = TRUE)
bodyfat_data$Age_Group <- cut(bodyfat_data$Age, 
                              breaks = age_breaks, 
                              labels = c("Young", "Middle-Aged", "Older", "Senior"), 
                              include.lowest = TRUE)
anova_result <- aov(BodyFat ~ Age_Group, data = bodyfat_data)
summary(anova_result)
TukeyHSD(anova_result)

#<---------Anova & Tukey Test end---------->

#9. Principal Component Analysis (PCA)
pca_data <- scale(bodyfat_data[, sapply(bodyfat_data, is.numeric)])
pca_result <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Saving Scree Plot (Variance Explained)
scree_data <- data.frame(PC = 1:length(pca_result$sdev), Variance = (pca_result$sdev)^2)

scree_plot <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "red") +
  labs(title = "Scree Plot of PCA", x = "Principal Component", y = "Variance Explained") +
  theme_minimal()

png("PCA_ScreePlot.png", width = 1200, height = 800)
print(scree_plot)
dev.off()

# Saving PCA Biplot
pca_df <- as.data.frame(pca_result$x)

pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_text(aes(label = rownames(bodyfat_data)), vjust = -1, size = 3) +
  labs(title = "PCA Biplot (PC1 vs PC2)", x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal()

png("PCA_Biplot.png", width = 1200, height = 800)
print(pca_plot)
dev.off()
#<----------------------PCA end-------------------->

#10. Hierarchical Clustering

# Computing Distance Matrix
dist_matrix <- dist(pca_data, method = "euclidean")

# Performing Hierarchical Clustering
hc_simple <- hclust(dist_matrix, method = "complete")

# Cutting Tree Into 3 Clusters
clusters <- cutree(hc_simple, k = 3)

# Converting Clusters to Data Frame
cluster_df <- as.data.frame(table(clusters))

# Saving Cluster Size Bar Chart as PNG
png("Cluster_Size_BarChart.png", width = 1000, height = 800)

# Creating Bar Chart for Cluster Sizes
ggplot(cluster_df, aes(x = clusters, y = Freq, fill = as.factor(clusters))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c")) +  # Custom colors
  labs(title = "Number of Observations per Cluster", x = "Cluster", y = "Count") +
  theme_minimal()

dev.off()
#<------end of PCA & H-Clustering-------->

#=========================================
# other relational Visualizations
#=========================================

# Saving Boxplot: Abdomen by Body Fat Category
png("Boxplot_Abdomen_vs_BodyFat.png", width = 1000, height = 800)
ggplot(bodyfat_data, aes(x = Bodyfat_Split, y = Abdomen, fill = Bodyfat_Split)) +
  geom_boxplot() +
  labs(title = "Abdomen Circumference by Body Fat Category", x = "Body Fat Category", y = "Abdomen (cm)")
dev.off()

# Saving Line Chart for Age Groups with Different Shades of Blue
blue_shades <- c("Young" = "#b3cde3",  # Light Blue
                 "Middle-Aged" = "#6497b1",  # Medium Blue
                 "Older" = "#005b96",  # Dark Blue
                 "Senior" = "#03396c")  # Deepest Blue

png("LineChart_AgeGroups_vs_BodyFat.png", width = 1000, height = 800)
ggplot(bodyfat_data, aes(x = Age_Group, y = BodyFat, group = 1, color = Age_Group)) +
  geom_line(size = 1.2) +  
  geom_point(size = 4) +  
  scale_color_manual(values = blue_shades) +  
  labs(title = "Mean Body Fat Across Age Groups", x = "Age Group", y = "Mean Body Fat (%)") +
  theme_minimal()
dev.off()

# Saving Scatter Plot: Abdomen vs BodyFat
png("Scatter_Abdomen_vs_BodyFat.png", width = 1000, height = 800)
ggplot(bodyfat_data, aes(x = Abdomen, y = BodyFat)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Plot: Abdomen vs BodyFat", x = "Abdomen (cm)", y = "Body Fat (%)")
dev.off()

# Saving Scatter Plot: Weight vs Hip
png("Scatter_Weight_vs_Hip.png", width = 1000, height = 800)
ggplot(bodyfat_data, aes(x = Weight, y = Hip)) +
  geom_point(color = "green", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Scatter Plot: Weight vs Hip", x = "Weight (lbs)", y = "Hip Circumference (cm)")
dev.off()

#<===============================================================================================>

# Saving Final Processed Data
write.csv(bodyfat_data, "BodyFat_Processed.csv", row.names = FALSE)