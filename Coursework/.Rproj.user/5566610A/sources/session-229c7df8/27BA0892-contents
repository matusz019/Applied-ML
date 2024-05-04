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
