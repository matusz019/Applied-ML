# Load the training dataset
train <- read.csv("concrete_strength_train.csv")

# Calculate relative frequency
train_freq <- prop.table(table(train$Strength))

# Summary statistics
summary_stats <- summary(train)
mean_value <- mean(train$Strength)
min_value <- min(train$Strength)
max_value <- max(train$Strength)
cov_value <- sd(train$Strength) / mean(train$Strength)  # Coefficient of variation

# Visual exploration of the Strength variable
library(ggplot2)
ggplot(train, aes(x = Strength, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  geom_text(data = data.frame(x = c(0.8 * max(train$Strength)), y = c(0.04),
                              label = paste("Mean:", round(mean_value, 2),
                                            "\nMin:", min_value, "\nMax:",
                                            max_value, "\nCOV:", round(cov_value, 2))),
            aes(x = x, y = y, label = label),
            color = "red", hjust = 0, vjust = 1) +
  labs(title = "Distribution of Concrete Strength in Training Dataset",
       x = "Concrete Strength (MPa)",
       y = "Relative Frequency",
       caption = "Figure 1.1") +
  theme(plot.caption = element_text(hjust = 0))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the testing dataset
test <- read.csv("concrete_strength_test.csv")

# Calculate relative frequency
test_freq <- prop.table(table(test$Strength))

# Summary statistics
summary_stats <- summary(test)
mean_value <- mean(test$Strength)
min_value <- min(test$Strength)
max_value <- max(test$Strength)
cov_value <- sd(test$Strength) / mean(test$Strength)  # Coefficient of variation

# Visual exploration of the Strength variable
ggplot(test, aes(x = Strength, y = ..density..)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  geom_text(data = data.frame(x = c(0.8 * max(test$Strength)), y = c(0.04),
                              label = paste("Mean:", round(mean_value, 2),
                                            "\nMin:", min_value, "\nMax:",
                                            max_value, "\nCOV:", round(cov_value, 2))),
            aes(x = x, y = y, label = label),
            color = "red", hjust = 0, vjust = 1) +
  labs(title = "Distribution of Concrete Strength in Testing Dataset",
       x = "Concrete Strength (MPa)",
       y = "Relative Frequency",
       caption = "Figure 1.1") +
  theme(plot.caption = element_text(hjust = 0))

#-------------------------------------------Calculating missingness------------------------------
# Load required libraries
library(readr)
library(mice)

# Load train and test datasets
train_data <- read_csv("concrete_strength_train.csv")
test_data <- read_csv("concrete_strength_test.csv")

# Function to handle missing values
handle_missing_values <- function(data) {
  # Examine missingness by column
  col_missingness <- colMeans(is.na(data))
  
  # Calculate average missingness
  avg_missingness <- mean(col_missingness) * 100  # Convert to percentage
  
  # Print average missingness
  cat("Average missingness across columns:", avg_missingness, "%\n")
  
  if (avg_missingness > 1) {
    # Impute missing values using mice
    cat("Imputing missing values using mice()...\n")
    imputed_data <- mice(data)
    return(imputed_data)
  } else {
    # Remove missing values
    cat("Removing missing values...\n")
    clean_data <- na.omit(data)
    return(clean_data)
  }
}

# Apply the function to train and test datasets
clean_train_data <- handle_missing_values(train_data)
clean_test_data <- handle_missing_values(test_data)

#---------------------------------------PCA Cluster-----------------------------
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
  geom_point(data = pca_scores_df, aes(x = Dim.1, y = Dim.2, color = Cluster),
             shape = 21, size = 3) +
  geom_point(data = cluster_centroids_df, aes(x = Dim.1, y = Dim.2, fill = Cluster),
             shape = 19, color = "black", size = 4) +
  labs(x = "PC1",
       y = "PC2",
       title = "PCA Scores Plot with Clusters and Centroids") +
  scale_fill_manual(values = c("red", "green", "blue")) +
  scale_color_manual(values = c("red", "green", "blue")) +
  theme_minimal()

#-------------------------------------Removing outliers-------------------------
# Load your dataset
trainData <- read.csv("concrete_strength_test.csv")
testData <- read.csv("concrete_strength_train.csv")

# Function to identify outliers based on z-scores for each column
identify_outliers <- function(column, threshold) {
  z_scores <- scale(column)  # Calculate z-scores
  outliers <- abs(z_scores) > threshold  # Identify outliers
  return(outliers)
}

# Set a threshold for identifying outliers (e.g., z-score > 2 or z-score > 3)
threshold <- 2

# Initialize a list to store outlier information for each column
outliers_list <- list()

# Loop through each column (excluding non-numeric columns)
for (col in names(trainData)) {
  if (is.numeric(trainData[[col]])) {
    outliers <- identify_outliers(trainData[[col]], threshold)  # Identify outliers for the column
    outliers_list[[col]] <- outliers  # Store outlier information in the list
  }
}

# Combine outlier information for all columns
combined_outliers <- Reduce("|", outliers_list)

# Remove outliers from the dataset
trainData_clean <- trainData[!combined_outliers, ]

# Print the cleaned dataset
print(trainData_clean)

# Loop through each column (excluding non-numeric columns)
for (col in names(testData)) {
  if (is.numeric(testData[[col]])) {
    outliers <- identify_outliers(testData[[col]], threshold)  # Identify outliers for the column
    outliers_list[[col]] <- outliers  # Store outlier information in the list
  }
}

# Combine outlier information for all columns
combined_outliers <- Reduce("|", outliers_list)

# Remove outliers from the dataset
testData_clean <- testData[!combined_outliers, ]
# Print the cleaned dataset
print(testData_clean)
#------------------------------Correlation matrix-------------------------------
# Select only predictor variables (exclude the target variable)
predictor_vars <- subset(trainData_clean, select = -c(Strength))

# Compute the correlation matrix
correlation_matrix <- cor(predictor_vars)

# Print the correlation matrix
print(correlation_matrix)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~PART 2~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#------------------------------Multi-Variable Linear Regression-----------------
# Load data
data <- read.csv("testData_clean")

# Fit the model
model <- lm(Strength ~ Cement + + Blast.Furnace.Slag + Fly.Ash + Water + Superplasticizer + Coarse.Aggregate + Fine.Aggregate + Age, data = testData_clean)

# Summarize the model
summary(model)

