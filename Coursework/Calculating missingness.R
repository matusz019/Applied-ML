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
