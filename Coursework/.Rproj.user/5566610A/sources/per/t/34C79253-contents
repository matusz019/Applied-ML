# Load required libraries
library(caret)
library(readr)
library(dplyr)
library(FactoMineR)
library(ggplot2)

# Load train and test datasets
train_data <- read_csv("concrete_strength_train.csv")
test_data <- read_csv("concrete_strength_test.csv")

# Combine train and test datasets
all_data <- bind_rows(train_data, test_data)

# Separate features and target
X_all <- all_data %>% select(-`Strength`)

# Standardize the features
preprocess <- preProcess(X_all, method = c("center", "scale"))
X_all_scaled <- predict(preprocess, X_all)

# Perform PCA
pca_result <- PCA(X_all_scaled)

# Extract PCA scores
pca_scores <- pca_result$ind$coord

# Perform clustering 
num_clusters <- 3  # Number of clusters
set.seed(123)  # For reproducibility
clusters <- kmeans(pca_scores, centers = num_clusters)

# Convert PCA scores matrix to data frame
pca_scores_df <- as.data.frame(pca_scores)
names(pca_scores_df) <- c("Dim.1", "Dim.2")

# Add cluster information to PCA scores data frame
pca_scores_df$Cluster <- as.factor(clusters$cluster)

# Calculate cluster centroids
cluster_centroids <- clusters$centers

# Convert cluster centroids to data frame and add Cluster column
cluster_centroids_df <- as.data.frame(cluster_centroids)
cluster_centroids_df$Cluster <- as.factor(1:num_clusters)  # Add Cluster column

# Plot PCA scores with clusters and centroids
ggplot() +
  geom_point(data = pca_scores_df, aes(x = Dim.1, y = Dim.2, color = Cluster), shape = 21, size = 3) +
  geom_point(data = cluster_centroids_df, aes(x = Dim.1, y = Dim.2, fill = Cluster), shape = 19, color = "black", size = 4) +
  labs(x = "PC1",
       y = "PC2",
       title = "PCA Scores Plot with Clusters and Centroids") +
  scale_fill_manual(values = c("red", "green", "blue")) +
  scale_color_manual(values = c("red", "green", "blue")) +
  theme_minimal()


